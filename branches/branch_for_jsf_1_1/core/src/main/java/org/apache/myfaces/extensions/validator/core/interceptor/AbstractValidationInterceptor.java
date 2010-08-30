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

import org.apache.myfaces.extensions.validator.core.el.ELHelper;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ValidationModuleKey;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.storage.RendererInterceptorPropertyStorage;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipBeforeInterceptorsException;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipRendererDelegationException;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipAfterInterceptorsException;
import org.apache.myfaces.extensions.validator.core.renderkit.RendererProxy;
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
 * A basic implementation of a RendererInterceptor for validating fields.  It adds some extension point for subclasses
 * and performs tasks like : <br/>
 * - storing field values (recordProcessedInformation) <br/>
 * - resetting required information property UIComponent <br/>
 * - calling before and after Validation interceptors <br/>
 * - etc ...
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.REUSE)
public abstract class AbstractValidationInterceptor extends AbstractRendererInterceptor
{
    private ELHelper elHelper;

    /**
     * Defines if the required option of UIComponents need to be set to false after a decode.  It is one of the
     * conditions that determine if the false is set.  see afterDecode method in this class.
     *
     * @return false when required property should not be reset to false.
     */
    protected boolean isRequiredInitializationSupported()
    {
        return false;
    }

    @Override
    /**
     * Sets required property of UIComponent to false after decoding when the isRequiredResetActivated and
     * isRequiredInitializationActive methods of ExtValUtils indicate that required property should be set and reset.
     *
     * @param facesContext The JSF Context
     * @param uiComponent  The component which is processed
     * @param renderer The renderer that will be called for the apply request values JSF phase.
     * @throws SkipAfterInterceptorsException Can be thrown to stop the execution of the afterDecode
     * methods of the registered interceptors.
     */
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
    /**
     * When the component should be processed and component initialization is not deactivated, the initialization of
     * the UIComponent is performed.  The initialization can be that the required and length properties are set based
     * on the annotations found on the property of the target property.
     *
     * @param facesContext The JSF Context
     * @param uiComponent The component which is processed
     * @param renderer The renderer that will be called for the render response JSF phase.
     * @throws IOException  In case the response writer is accessed and there was an IO problem.
     * @throws SkipBeforeInterceptorsException Can be thrown to stop the execution of the beforeEncodeBegin methods of
     * the registered interceptors.
     * @throws SkipRendererDelegationException Can be thrown to stop the execution of the beforeEncodeBegin method.
     */
    public void beforeEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
            throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
        if(processComponent(uiComponent) && !isComponentInitializationDeactivated())
        {
            initComponent(facesContext, uiComponent);
        }
    }

    /**
     * Initialize the component based on the annotations found on the property of the target property. The properties
     * that can be set are for instance the length and required attribute of the Component.
     *
     * @param facesContext The JSF Context
     * @param uiComponent The component which is processed
     */
    protected abstract void initComponent(FacesContext facesContext, UIComponent uiComponent);

    @Override
    /**
     * The method performs the validation of the field and calls the registered interceptors regarding the validation.
     * The main steps are :<br/>
     * - Get the converted value from the renderer (possibly cached by the RendererProxy)<br/>
     * - Store the value for use in the cross validation options <br/>
     * - Adjust the converted value for interpret empty values as null.<br/>
     * - Execute the beforeValidation method of the registered PropertyValidationInterceptor's. <br/>
     * - Perform the validation when the PropertyValidationInterceptor have indicate it that it should be performed.
     * <br/>
     * - When a validation error occurred, ask the ViolationSeverityInterpreter if this validation should result in an
     *  exception <br/>
     * - Execute the afterValidation method of the registered PropertyValidationInterceptor's. (when validation actually
     *  tooks place)
     * @param facesContext The JSF Context
     * @param uiComponent  The component which is processed
     * @param submittedValue The submitted value
     * @param renderer The renderer that will be called for the apply request values JSF phase.
     * @throws ConverterException ExtVal validation strategies can throw ValidationExceptions which are converted by
     * AbstractValidationInterceptor
     * @throws SkipBeforeInterceptorsException Can be thrown to stop the execution of the beforeGetConvertedValue
     * methods of the registered interceptors.
     * @throws SkipRendererDelegationException Can be thrown to stop the execution of the beforeGetConvertedValue
     * method.
     */
    public void beforeGetConvertedValue(FacesContext facesContext, UIComponent uiComponent, Object o, Renderer wrapped)
            throws ConverterException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
        Object convertedObject;

        try
        {
            if(wrapped instanceof RendererProxy)
            {
                convertedObject = ((RendererProxy)wrapped).getCachedConvertedValue(facesContext, uiComponent, o);
            }
            else
            {
                convertedObject = wrapped.getConvertedValue(facesContext, uiComponent, o);
            }
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
            //record user input e.g. for cross-component validation
            for(ProcessedInformationRecorder recorder : ExtValContext.getContext().getProcessedInformationRecorders())
            {
                recorder.recordUserInput(uiComponent, convertedObject);

                logger.finest(recorder.getClass().getName() + " called");
            }
        }

        boolean validateValue = false;
        try
        {
            if(processComponent(uiComponent))
            {
                convertedObject = transformValueForValidation(convertedObject);

                validateValue = validateValue(convertedObject);
                if(validateValue && processBeforeValidation(facesContext, uiComponent, convertedObject))
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
            if(validateValue)
            {
                processAfterValidation(facesContext, uiComponent, convertedObject);
            }
            resetRendererInterceptorProperties(uiComponent);
        }
    }

    /**
     * Execute the beforeValidation method of the registered PropertyValidationInterceptor's.
     *
     * @param facesContext The JSF Context
     * @param uiComponent The UIComponent which is processed.
     * @param value The value to validate
     * @return true when validation can proceed, false otherwise.
     */
    protected boolean processBeforeValidation(FacesContext facesContext, UIComponent uiComponent, Object value)
    {
        return ExtValUtils.executeGlobalBeforeValidationInterceptors(facesContext, uiComponent, value,
                PropertyInformation.class.getName(), getPropertyInformation(facesContext, uiComponent), getModuleKey());
    }

    /**
     * Execute the afterValidation method of the registered PropertyValidationInterceptor's.
     *
     * @param facesContext The JSF Context
     * @param uiComponent The UIComponent which is processed.
     * @param value The value which has just been validated.
     */
    protected void processAfterValidation(FacesContext facesContext, UIComponent uiComponent, Object value)
    {
        ExtValUtils.executeGlobalAfterValidationInterceptors(facesContext, uiComponent, value,
                PropertyInformation.class.getName(), getPropertyInformation(facesContext, uiComponent), getModuleKey());
    }

    /**
     * Get the property information of the UIComponent from the MetaDataExtractor.
     *
     * @param facesContext The JSF Context
     * @param uiComponent The UIComponent which is processed.
     * @return All the information about the UIComponent (like annotations, custom info from Extractor, etc)
     */
    protected PropertyInformation getPropertyInformation(FacesContext facesContext, UIComponent uiComponent)
    {
        Map<String, Object> properties = getPropertiesForComponentMetaDataExtractor(uiComponent);

        MetaDataExtractor metaDataExtractor = getComponentMetaDataExtractor(properties);

        return metaDataExtractor.extract(facesContext, uiComponent);
    }

    /**
     * Create the properties which will be used by the selection of the MetaDataExtractor.  By default it adds the
     * ModuleKey (if available) and the UIComponent.
     *
     * @param uiComponent  The UIComponent which is processed.
     * @return properties used by the selection of the MetaDataExtractor
     */
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

    /**
     * Implementations must return the MetaDataExtractor that will perform the extraction of the meta data from the
     * component.  The component itself is present in the properties map when it would influence the type of Extractor
     * which is returned.
     *
     * @param properties Properties that can be used to determine the extractor which is returned.
     * @return The MetaDataExtractor used for performing the xtraction of the meta data from the component.
     */
    protected abstract MetaDataExtractor getComponentMetaDataExtractor(Map<String, Object> properties);

    /**
     * Converts an empty String to null when the parameter interpretEmptyStringValuesAsNull is set.
     *
     * @param convertedObject  Converted objected
     * @return Adjusted value that should be used from now on as converted value.
     */
    protected Object transformValueForValidation(Object convertedObject)
    {
        if ("".equals(convertedObject) && interpretEmptyStringValuesAsNull())
        {
            return null;
        }

        return convertedObject;
    }

    /**
     * Determines if the value should be validated in case it is empty (null or no characters in string). If it happens
     * to be empty and the configuration indicates that we don't need to validate such values, the method return false
     * and no ExtVal validations will be performed.
     *
     * @param convertedObject The converted value.
     * @return Should the value be converted?
     */
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

    /**
     * Defines if a value is empty. The definition of empty is that it is null or has no characters in the String value.
     * @param convertedObject The converted value.
     * @return is the value empty?
     */
    protected boolean isValueToValidateEmpty(Object convertedObject)
    {
        return convertedObject == null || "".equals(convertedObject);
    }

    /**
     * Check if empty fields should be validated based on the configuration parameter javax.faces.VALIDATE_EMPTY_FIELDS.
     * @return  Do we need to validate empty fields.
     */
    protected boolean validateEmptyFields()
    {
        return ExtValUtils.validateEmptyFields();
    }

    /**
     * Check if empty string values should be interpret as null based on the configuration parameter
     * javax.faces.INTERPRET_EMPTY_STRING_SUBMITTED_VALUES_AS_NULL.
     *
     * @return Do we need to interpret empty String values as null.
     */
    protected boolean interpretEmptyStringValuesAsNull()
    {
        return ExtValUtils.interpretEmptyStringValuesAsNull();
    }

    /**
     * Perform the actual validation of the value.
     *
     * @param facesContext The JSF Context
     * @param uiComponent The UIComponent which is processed.
     * @param convertedObject The adjusted converted value.
     */
    protected abstract void processValidation(
            FacesContext facesContext, UIComponent uiComponent, Object convertedObject);

    /**
     * Determines if the value of the UIComponent needs to be processed.  By default it is so when the component is a
     * EditableValueHolder and the value binding can be interpreted.
     *
     * @param uiComponent The UIComponent which is processed.
     * @return Do we need to process the UIComponent
     */
    protected boolean processComponent(UIComponent uiComponent)
    {
        return uiComponent instanceof EditableValueHolder && isValueBindingOfComponentValid(uiComponent);
    }

    /**
     * Returns the ELHelper to be used in the process of the validation. It is cached for performance reasons.
     * @return The ELHelper.
     */
    protected ELHelper getELHelper()
    {
        if(this.elHelper == null)
        {
            this.elHelper = ExtValUtils.getELHelper();
        }
        return this.elHelper;
    }

    /**
     * Determines if the value binding of the component is valid. That is, it can be processed by the ELHelper.
     * {@see org.apache.myfaces.extensions.validator.core.el.ELHelper#getPropertyDetailsOfValueBinding(javax.faces.component.UIComponent)}
     * @param uiComponent The UIComponent which is processed.
     * @return Is the value binding correct, interpretable by ExtVal.
     */
    private boolean isValueBindingOfComponentValid(UIComponent uiComponent)
    {
        try
        {
            return getELHelper().getPropertyDetailsOfValueBinding(uiComponent) != null;
        }
        catch (Exception e)
        {
            return false;
        }
    }

    /**
     * Checks from the configuration if the component initialization is deactivated.
     * @return Is the component initialization deactivated.
     */
    private boolean isComponentInitializationDeactivated()
    {
        return ExtValCoreConfiguration.get().deactivateComponentInitialization();
    }

    /**
     * Will the converted value be stored for the cross validation feature. By default it is not stored.
     * @return will the the converted value be stored.
     */
    protected boolean recordProcessedInformation()
    {
        //override if needed
        return false;
    }

    /**
     * Identification of the validation module.  By default it returns null so no module is assigned.
     * @return Identification of the validation module.
     */
    protected Class getModuleKey()
    {
        //override if needed
        return null;
    }

    /**
     * Returns the properties which will be made available to interceptors. By default the moduleKey and the UIComponent
     * itself is added.
     *
     * @param uiComponent  The UIComponent which is processed.
     * @return  properties for the interceptors.
     */
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

    /**
     * Store the interceptor properties in the RendererInterceptorPropertyStorage.
     *
     * @param uiComponent The UIComponent which is processed.
     */
    private void setRendererInterceptorProperties(UIComponent uiComponent)
    {
        RendererInterceptorPropertyStorage interceptorPropertyStorage = getRendererInterceptorPropertyStorage();

        Map<String, Object> properties = getInterceptorProperties(uiComponent);
        for(Map.Entry<String, Object> entry : properties.entrySet())
        {
            interceptorPropertyStorage.setProperty(entry.getKey(), entry.getValue());
        }
    }

    /**
     * remove the interceptor properties from the RendererInterceptorPropertyStorage.
     * @param uiComponent The UIComponent which is processed.
     */
    private void resetRendererInterceptorProperties(UIComponent uiComponent)
    {
        RendererInterceptorPropertyStorage interceptorPropertyStorage = getRendererInterceptorPropertyStorage();

        for(String key : getInterceptorProperties(uiComponent).keySet())
        {
            interceptorPropertyStorage.removeProperty(key);
        }
    }

    /**
     * Retrieves the RendererInterceptorPropertyStorage defined in the ExtVal system.
     *
     * @return The RendererInterceptorPropertyStorage
     */
    private RendererInterceptorPropertyStorage getRendererInterceptorPropertyStorage()
    {
        return ExtValUtils.getStorage(RendererInterceptorPropertyStorage.class,
                RendererInterceptorPropertyStorage.class.getName());
    }
}
