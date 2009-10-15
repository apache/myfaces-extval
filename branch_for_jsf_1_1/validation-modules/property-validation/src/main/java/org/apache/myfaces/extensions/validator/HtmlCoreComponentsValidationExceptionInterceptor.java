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
package org.apache.myfaces.extensions.validator;

import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.interceptor.ValidationExceptionInterceptor;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.message.LabeledMessage;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ViolationSeverity;
import org.apache.myfaces.extensions.validator.core.InvocationOrder;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.component.UIComponent;
import javax.faces.component.EditableValueHolder;
import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlInputSecret;
import javax.faces.component.html.HtmlSelectBooleanCheckbox;
import javax.faces.component.html.HtmlSelectOneListbox;
import javax.faces.component.html.HtmlSelectOneMenu;
import javax.faces.component.html.HtmlSelectOneRadio;
import javax.faces.component.html.HtmlSelectManyCheckbox;
import javax.faces.component.html.HtmlSelectManyListbox;
import javax.faces.component.html.HtmlSelectManyMenu;
import javax.faces.component.html.HtmlInputTextarea;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;
import javax.faces.application.FacesMessage;
import java.lang.annotation.Annotation;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@InvocationOrder(200)
@UsageInformation(UsageCategory.INTERNAL)
public class HtmlCoreComponentsValidationExceptionInterceptor implements ValidationExceptionInterceptor
{
    protected final Log logger = LogFactory.getLog(getClass());

    public boolean afterThrowing(UIComponent uiComponent,
                                 MetaDataEntry metaDataEntry,
                                 Object convertedObject,
                                 ValidatorException validatorException,
                                 ValidationStrategy validatorExceptionSource)
    {
        if(processComponent(uiComponent))
        {
            FacesContext facesContext = FacesContext.getCurrentInstance();
            FacesMessage facesMessage = ExtValUtils.convertFacesMessage(validatorException.getFacesMessage());

            String label = (String) ReflectionUtils.tryToInvokeMethod(uiComponent,
                ReflectionUtils.tryToGetMethod(uiComponent.getClass(), "getLabel"));

            if(label == null)
            {
                label = uiComponent.getClientId(facesContext);
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

            if(metaDataEntry != null && metaDataEntry.getValue() instanceof Annotation)
            {
                if(!displayAsException(facesMessage, metaDataEntry.getValue(Annotation.class)))
                {
                    facesContext.addMessage(uiComponent.getClientId(facesContext), facesMessage);
                    //it's a special case - since validation will continue it's essential to reset it
                    ((EditableValueHolder)uiComponent).setRequired(false);
                    return false;
                }
            }
        }
        return true;
    }

    @ToDo(value = Priority.MEDIUM, description = "refactor to a generic parameter extractor")
    private boolean displayAsException(FacesMessage facesMessage, Annotation annotation)
    {
        boolean isError = true;

        for(FacesMessage.Severity severity : ExtValUtils.getValidationParameterExtractor()
                .extract(annotation, ViolationSeverity.class, FacesMessage.Severity.class))
        {
            if(severity.compareTo(facesMessage.getSeverity()) < 0)
            {
                facesMessage.setSeverity(severity);
                isError = false;
            }
        }

        return isError;
    }

    protected boolean processComponent(UIComponent uiComponent)
    {
        return uiComponent instanceof HtmlInputText ||
                uiComponent instanceof HtmlInputSecret ||
                uiComponent instanceof HtmlSelectBooleanCheckbox ||
                uiComponent instanceof HtmlSelectOneListbox ||
                uiComponent instanceof HtmlSelectOneMenu ||
                uiComponent instanceof HtmlSelectOneRadio ||
                uiComponent instanceof HtmlSelectManyCheckbox ||
                uiComponent instanceof HtmlSelectManyListbox ||
                uiComponent instanceof HtmlSelectManyMenu ||
                uiComponent instanceof HtmlInputTextarea;
    }
}
