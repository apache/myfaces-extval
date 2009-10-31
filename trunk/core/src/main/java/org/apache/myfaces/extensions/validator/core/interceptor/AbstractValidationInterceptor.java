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
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipBeforeInterceptorsException;
import org.apache.myfaces.extensions.validator.core.renderkit.exception.SkipRendererDelegationException;
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

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.REUSE)
public abstract class AbstractValidationInterceptor extends AbstractRendererInterceptor
{
    @Override
    @ToDo(value = Priority.MEDIUM, description = "add web.xml context param to deactivate component initialization")
    public void beforeEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
            throws IOException, SkipBeforeInterceptorsException, SkipRendererDelegationException
    {
        if(processComponent(uiComponent))
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
            if(this.logger.isFatalEnabled())
            {
                this.logger.fatal("it seems you are using an invalid binding. " + wrapped.getClass().getName()
                        + ": conversion failed. normally this is >not< a myfaces extval issue!", r);
            }

            throw r;
        }

        if(recordProcessedInformation())
        {
            //recorde user input e.g. for cross-component validation
            for(ProcessedInformationRecorder recorder : ExtValContext.getContext().getProcessedInformationRecorders())
            {
                recorder.recordUserInput(uiComponent, convertedObject);

                if(logger.isTraceEnabled())
                {
                    logger.trace(recorder.getClass().getName() + " called");
                }
            }
        }

        try
        {
            if(processComponent(uiComponent))
            {
                convertedObject = transformValueForValidation(convertedObject);

                if(validateValue(convertedObject))
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
    }

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
            if(this.logger.isDebugEnabled())
            {
                this.logger.debug("empty field validation is deactivated in the web.xml - see: " +
                        "javax.faces.VALIDATE_EMPTY_FIELDS");
            }

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
        return uiComponent instanceof EditableValueHolder &&
                !isComponentInitializationDeactivated() && isValueBindingOfComponentValid(uiComponent);
    }

    private boolean isValueBindingOfComponentValid(UIComponent uiComponent)
    {
        try
        {
            return ExtValUtils.getELHelper().getPropertyDetailsOfValueBinding(uiComponent) != null;
        }
        catch (Throwable t)
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
}
