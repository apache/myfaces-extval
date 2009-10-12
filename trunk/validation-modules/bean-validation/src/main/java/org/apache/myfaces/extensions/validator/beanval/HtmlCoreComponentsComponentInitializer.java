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
package org.apache.myfaces.extensions.validator.beanval;

import org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer;
import org.apache.myfaces.extensions.validator.core.metadata.CommonMetaDataKeys;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.component.EditableValueHolder;
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
import java.util.Map;

/**
 * keep in sync with: org.apache.myfaces.extensions.validator.HtmlCoreComponentsComponentInitializer
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class HtmlCoreComponentsComponentInitializer implements ComponentInitializer
{
    public void configureComponent(FacesContext facesContext, UIComponent uiComponent, Map<String, Object> metaData)
    {
        if(processComponent(uiComponent))
        {
            configureRequiredAttribute(facesContext, uiComponent, metaData);
            configureMaxLengthAttribute(facesContext, uiComponent, metaData);
        }
    }

    protected void configureRequiredAttribute(FacesContext facesContext,
                                              UIComponent uiComponent,
                                              Map<String, Object> metaData)
    {
        if(!ExtValUtils.interpretEmptyStringValuesAsNull())
        {
            return;
        }

        if(Boolean.TRUE.equals(metaData.get(CommonMetaDataKeys.REQUIRED)) ||
                Boolean.TRUE.equals(isComponentRequired(uiComponent)))
        {
            ((EditableValueHolder)uiComponent).setRequired(true);
        }
        else
        {
            ((EditableValueHolder)uiComponent).setRequired(false);
        }
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

    /**
     * if there is no special attribute at the component which should overrule
     * the annotated property return true!
     *
     * @param uiComponent component which implements the EditableValueHolder interface
     * @return false to overrule the annotated property e.g. if component is readonly
     */
    protected Boolean isComponentRequired(UIComponent uiComponent)
    {
        boolean isReadOnly = !Boolean.FALSE.equals(ReflectionUtils.tryToInvokeMethod(
                uiComponent, ReflectionUtils.tryToGetMethod(uiComponent.getClass(), "isReadonly")));
        boolean isDisabled = !Boolean.FALSE.equals(ReflectionUtils.tryToInvokeMethod(
                uiComponent, ReflectionUtils.tryToGetMethod(uiComponent.getClass(), "isDisabled")));

        return !(isReadOnly || isDisabled);
    }

    protected void configureMaxLengthAttribute(FacesContext facesContext,
                                             UIComponent uiComponent,
                                             Map<String, Object> metaData)
    {
        if(metaData.containsKey(CommonMetaDataKeys.MAX_LENGTH))
        {
            Object maxLength = metaData.get(CommonMetaDataKeys.MAX_LENGTH);

            if(!(maxLength instanceof Integer))
            {
                return;
            }
            if(uiComponent instanceof HtmlInputText)
            {
                HtmlInputText htmlInputText = (HtmlInputText)uiComponent;
                htmlInputText.setMaxlength((Integer)maxLength);
            }
        }
    }
}