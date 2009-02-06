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
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.component.UIComponent;
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

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class HtmlCoreComponentsValidationExceptionInterceptor implements ValidationExceptionInterceptor
{
    public boolean afterThrowing(UIComponent uiComponent,
                                 MetaDataEntry metaDataEntry,
                                 Object convertedObject,
                                 ValidatorException validatorException,
                                 ValidationStrategy validatorExceptionSource)
    {
        if(processComponent(uiComponent) && metaDataEntry != null)
        {
            FacesMessage facesMessage = validatorException.getFacesMessage();

            String label = (String) ReflectionUtils.tryToInvokeMethod(uiComponent,
                ReflectionUtils.tryToGetMethod(uiComponent.getClass(), "getLabel"));

            if(label == null)
            {
                label = uiComponent.getClientId(FacesContext.getCurrentInstance());
            }

            //override the label if the annotation provides a label
            if(metaDataEntry.getProperty(PropertyInformationKeys.LABEL) != null)
            {
                label = metaDataEntry.getProperty(PropertyInformationKeys.LABEL, String.class);
            }

            for(int i = 0; i < 3; i++)
            {
                ExtValUtils.tryToPlaceLabel(facesMessage, label, i);
            }
        }
        return true;
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