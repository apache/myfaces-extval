/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.core.interceptor;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.ValidationModuleKey;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.storage.RendererInterceptorPropertyStorage;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipBeforeInterceptorsException;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipRendererDelegationException;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipAfterInterceptorsException;
import org.apache.myfaces.extensions.validator.core.recorder.ProcessedInformationRecorder;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import javax.faces.component.EditableValueHolder;
import javax.faces.convert.ConverterException;
import javax.faces.render.Renderer;
import javax.faces.validator.ValidatorException;
import javax.el.PropertyNotFoundException;
import java.io.IOException;
import java.util.Map;
import java.util.HashMap;
import java.util.logging.Level;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.REUSE)
public abstract class AbstractValidationInterceptor extends AbstractRendererInterceptor
{
    protected boolean isRequiredInitializationSupported()
    {
        return false;
    }

    @Override
    public void afterDecode(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
            throws SkipAfterInterceptorsException
    {
        /*
         * component initialization sets a component to required if there are constraints which indicate it
         * the required flag in a component leads to problems with h:messages (additional message) as well as
         * incompatibilities with skip validation and severities
         */
        if(uiComponent instanceof EditableValueHolder && ExtValUtils.isRequiredResetActivated() &&
                isRequiredInitializationSupported() && ExtValUtils.isRequiredInitializationActive())
        {
            ((EditableValueHolder)uiComponent).setRequired(false);
        }
    }

    @Override
    public void beforeEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
            throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
        if(processComponent(uiComponent) && !isComponentInitializationDeactivated())
        {
            initComponent(facesContext, uiComponent);
        }
    }

    protected abstract void initComponent(FacesContext facesContext, UIComponent uiComponent);

    @Override
    public void beforeGetConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o, Renderer wrapped)
            throws ConverterException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
        Object convertedObject;

        try
        {
            convertedObject = wrapped.getConvertedValue(facesContext, uiComponent, o);
        }
        catch (PropertyNotFoundException r)
        {
            this.logger.log(Level.SEVERE, "it seems you are using an invalid binding. " + wrapped.getClass().getName()
                    + ": conversion failed. normally this is >not< a myfaces extval issue!", r);

            throw r;
        }

        setRendererInterceptorProperties(uiComponent);
        
        if(recordProcessedInformation())
        {
            //recorde user input e.g. for cross-component validation
            for(ProcessedInformationRecorder recorder : ExtValContext.getContext().getProcessedInformationRecorders())
            {
                recorder.recordUserInput(uiComponent, convertedObject);

                logger.finest(recorder.getClass().getName() + " called");
            }
        }

        try
        {
            if(processComponent(uiComponent))
            {
                convertedObject = transformValueForValidation(convertedObject);

                if(validateValue(convertedObject) &&
                        processBeforeValidation(facesContext, uiComponent, convertedObject))
                {
                    processValidation(facesContext, uiComponent, convertedObject);
                }
            }
        }
        catch (ValidatorException e)
        {
            try
            {
                //ViolationSeverityInterpreter might decide that it isn't an exception
                ExtValUtils.tryToThrowValidatorExceptionForComponent(uiComponent, e.getFacesMessage(), e);
            }
            catch (ValidatorException finalException)
            {
                throw new ConverterException(e.getFacesMessage(), e);
            }
        }
        finally
        {
            processAfterValidation(facesContext, uiComponent, convertedObject);
            resetRendererInterceptorProperties(uiComponent);
        }
    }

    protected boolean processBeforeValidation(FacesContext facesContext, UIComponent uiComponent, Object value)
    {
        return ExtValUtils.executeGlobalBeforeValidationInterceptors(facesContext, uiComponent, value,
                PropertyInformation.class.getName(), getPropertyInformation(facesContext, uiComponent), getModuleKey());
    }

    protected void processAfterValidation(FacesContext facesContext, UIComponent uiComponent, Object value)
    {
        ExtValUtils.executeGlobalAfterValidationInterceptors(facesContext, uiComponent, value,
                PropertyInformation.class.getName(), getPropertyInformation(facesContext, uiComponent), getModuleKey());
    }

    protected PropertyInformation getPropertyInformation(FacesContext facesContext, UIComponent uiComponent)
    {
        Map<String, Object> properties = getPropertiesForComponentMetaDataExtractor(uiComponent);

        MetaDataExtractor metaDataExtractor = getComponentMetaDataExtractor(properties);

        return metaDataExtractor.extract(facesContext, uiComponent);
    }

    protected Map<String, Object> getPropertiesForComponentMetaDataExtractor(UIComponent uiComponent)
    {
        Map<String, Object> properties = new HashMap<String, Object>();

        if(getModuleKey() != null)
        {
            properties.put(ValidationModuleKey.class.getName(), getModuleKey());
        }
        properties.put(UIComponent.class.getName(), uiComponent);
        return properties;
    }

    protected abstract MetaDataExtractor getComponentMetaDataExtractor(Map<String, Object> properties);

    protected Object transformValueForValidation(Object convertedObject)
    {
        if ("".equals(convertedObject) && interpretEmptyStringValuesAsNull())
        {
            return null;
        }

        return convertedObject;
    }

    protected boolean validateValue(Object convertedObject)
    {
        if(isValueToValidateEmpty(convertedObject) && !validateEmptyFields())
        {
            this.logger.fine("empty field validation is deactivated in the web.xml - see: " +
                    "javax.faces.VALIDATE_EMPTY_FIELDS");

            return false;
        }

        return true;
    }

    protected boolean isValueToValidateEmpty(Object convertedObject)
    {
        return convertedObject == null || "".equals(convertedObject);
    }

    protected boolean validateEmptyFields()
    {
        return ExtValUtils.validateEmptyFields();
    }

    protected boolean interpretEmptyStringValuesAsNull()
    {
        return ExtValUtils.interpretEmptyStringValuesAsNull();
    }

    protected abstract void processValidation(
            FacesContext facesContext, UIComponent uiComponent, Object convertedObject);

    protected boolean processComponent(UIComponent uiComponent)
    {
        return uiComponent instanceof EditableValueHolder && isValueBindingOfComponentValid(uiComponent);
    }

    private boolean isValueBindingOfComponentValid(UIComponent uiComponent)
    {
        try
        {
            return ExtValUtils.getELHelper().getPropertyDetailsOfValueBinding(uiComponent) != null;
        }
        catch (Exception e)
        {
            return false;
        }
    }

    private boolean isComponentInitializationDeactivated()
    {
        return "true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_COMPONENT_INITIALIZATION);
    }

    protected boolean recordProcessedInformation()
    {
        //override if needed
        return false;
    }

    protected Class getModuleKey()
    {
        //override if needed
        return null;
    }

    protected Map<String, Object> getInterceptorProperties(UIComponent uiComponent)
    {
        Map<String, Object> result = new HashMap<String, Object>();

        if(getModuleKey() != null)
        {
            result.put(ValidationModuleKey.class.getName(), getModuleKey());
        }
        result.put(UIComponent.class.getName(), uiComponent);

        return result;
    }

    private void setRendererInterceptorProperties(UIComponent uiComponent)
    {
        RendererInterceptorPropertyStorage interceptorPropertyStorage = getRendererInterceptorPropertyStorage();

        Map<String, Object> properties = getInterceptorProperties(uiComponent);
        for(Map.Entry<String, Object> entry : properties.entrySet())
        {
            interceptorPropertyStorage.setProperty(entry.getKey(), entry.getValue());
        }
    }

    private void resetRendererInterceptorProperties(UIComponent uiComponent)
    {
        RendererInterceptorPropertyStorage interceptorPropertyStorage = getRendererInterceptorPropertyStorage();

        for(String key : getInterceptorProperties(uiComponent).keySet())
        {
            interceptorPropertyStorage.removeProperty(key);
        }
    }

    private RendererInterceptorPropertyStorage getRendererInterceptorPropertyStorage()
    {
        return ExtValUtils.getStorage(RendererInterceptorPropertyStorage.class,
                RendererInterceptorPropertyStorage.class.getName());
    }
}
