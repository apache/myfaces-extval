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
package org.apache.myfaces.extensions.validator.trinidad.interceptor;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.interceptor.ValidationExceptionInterceptor;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.message.LabeledMessage;
import org.apache.myfaces.extensions.validator.core.validation.exception.RequiredValidatorException;
import org.apache.myfaces.extensions.validator.core.InvocationOrder;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.trinidad.context.RequestContext;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import javax.faces.validator.ValidatorException;
import javax.faces.application.FacesMessage;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@InvocationOrder(300)
@UsageInformation(UsageCategory.INTERNAL)
public class TrinidadValidationExceptionInterceptor implements ValidationExceptionInterceptor
{
    private static final String TRINIDAD_CORE_INPUT_TEXT
                                         = "org.apache.myfaces.trinidad.component.core.input.CoreInputText";
    private static final String TRINIDAD_CORE_INPUT_DATE
                                         = "org.apache.myfaces.trinidad.component.core.input.CoreInputDate";

    public boolean afterThrowing(UIComponent uiComponent,
                                 MetaDataEntry metaDataEntry,
                                 Object convertedObject,
                                 ValidatorException validatorException,
                                 ValidationStrategy validatorExceptionSource)
    {
        refreshComponent(uiComponent);

        if(processComponent(uiComponent))
        {
            FacesMessage facesMessage = validatorException.getFacesMessage();

            handleRequiredValidatorException(uiComponent, validatorException);

            String label = getLabel(uiComponent);

            if(label == null)
            {
                label = uiComponent.getClientId(FacesContext.getCurrentInstance());
            }

            //override the label if the annotation provides a label
            if(metaDataEntry != null && metaDataEntry.getProperty(PropertyInformationKeys.LABEL) != null)
            {
                label = metaDataEntry.getProperty(PropertyInformationKeys.LABEL, String.class);
            }

            if(facesMessage instanceof LabeledMessage)
            {
                ((LabeledMessage)facesMessage).setLabelText(label);
            }
            //if someone uses a normal faces message
            else
            {
                for(int i = 0; i < 3; i++)
                {
                    ExtValUtils.tryToPlaceLabel(facesMessage, label, i);
                }
            }
        }
        return true;
    }

    private void handleRequiredValidatorException(UIComponent uiComponent, ValidatorException validatorException)
    {
        if(validatorException instanceof RequiredValidatorException)
        {
            FacesMessage facesMessage = validatorException.getFacesMessage();
            String inlineMessage = getInlineRequiredMessage(uiComponent);

            if(inlineMessage != null)
            {
                facesMessage.setSummary(inlineMessage);
                facesMessage.setDetail(inlineMessage);
            }
        }
    }

    private void refreshComponent(UIComponent uiComponent)
    {
        if(RequestContext.getCurrentInstance().isPartialRequest(FacesContext.getCurrentInstance()))
        {
            RequestContext.getCurrentInstance().addPartialTarget(uiComponent);
        }
    }

    protected boolean processComponent(UIComponent uiComponent)
    {
        return TRINIDAD_CORE_INPUT_TEXT.equals(uiComponent.getClass().getName()) ||
               TRINIDAD_CORE_INPUT_DATE.equals(uiComponent.getClass().getName());
    }

    private String getLabel(UIComponent uiComponent)
    {
        return (String)ReflectionUtils.tryToInvokeMethod(uiComponent,
                ReflectionUtils.tryToGetMethod(uiComponent.getClass(), "getLabel"));
    }

    private String getInlineRequiredMessage(UIComponent uiComponent)
    {
        return (String)ReflectionUtils.tryToInvokeMethod(uiComponent,
                ReflectionUtils.tryToGetMethod(uiComponent.getClass(), "getRequiredMessageDetail"));
    }
}
