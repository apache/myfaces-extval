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
package org.apache.myfaces.extensions.validator.core.initializer.component;

import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;
import org.apache.myfaces.extensions.validator.core.metadata.CommonMetaDataKeys;

import javax.faces.context.FacesContext;
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
import java.util.Map;

/**
 * Basic implementation of a {@link ComponentInitializer} which allows an easier handling of required validations and
 * provides a default implementation for standard components.
 *
 * @since x.x.3
 */
@UsageInformation(UsageCategory.REUSE)
public abstract class AbstractHtmlCoreComponentsComponentInitializer implements ComponentInitializer
{
    //short because it influences the state
    protected static final String INITIAL_MARKUP_META_DATA_KEY = "OAM_EV_MARKUP_METADATA";

    protected Boolean forceComponentInitialization;

    /**
     * If the component is one of the standard input components, the max length attribute is configured and the
     * required attribute is configured (if empty field validation and required initialization is activated)
     *
     * @param facesContext The JSF Context
     * @param uiComponent The component which should be initialised
     * @param metaData map which contains the transformed meta-data
     */
    public void configureComponent(FacesContext facesContext, UIComponent uiComponent, Map<String, Object> metaData)
    {
        if(processComponent(uiComponent))
        {
            if(validateEmptyFields() && isRequiredInitializationActive())
            {
                configureRequiredAttribute(facesContext, uiComponent, metaData);
            }

            configureMaxLengthAttribute(facesContext, uiComponent, metaData);
        }
    }

    /**
     * Uses the config introduced by JSF 2 for specifying if
     * fields without content should be available for required validation.
     *
     * @return true if fields with empty values should be validated, false otherwise.
     */
    protected boolean validateEmptyFields()
    {
        return ExtValUtils.validateEmptyFields();
    }

    /**
     * Uses the config introduced by prev. ExtVal versions for specifying if
     * required initialization should be used
     * (it's useful e.g. for client-side validations provided by libs like trinidad).
     *
     * @return true if ExtVal should transfer meta-data for required fields to the component
     */
    protected boolean isRequiredInitializationActive()
    {
        return ExtValUtils.isRequiredInitializationActive();
    }

    /**
     * The concrete implementation has to initialize the component based on the given meta-data map.
     * This method is only called if all pre-conditions are fulfilled.
     *
     * @param facesContext The JSF Context
     * @param uiComponent The UIComponent which should be configured.
     * @param metaData map which contains the transformed meta-data
     */
    protected abstract void configureRequiredAttribute(FacesContext facesContext,
                                                       UIComponent uiComponent,
                                                       Map<String, Object> metaData);

    /**
     * Activates the implementation for special components (-> other components will be ignored).
     *
     * @param uiComponent The UIComponent which should be configured.
     * @return Should the component be initialized.
     */
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
    @ToDo(value = Priority.MEDIUM, description = "refactor")
    protected boolean isRequiredInitializationSupported(UIComponent uiComponent)
    {
        boolean isReadOnly = !Boolean.FALSE.equals(ReflectionUtils.tryToInvokeMethod(
                uiComponent, ReflectionUtils.tryToGetMethod(uiComponent.getClass(), "isReadonly")));
        boolean isDisabled = !Boolean.FALSE.equals(ReflectionUtils.tryToInvokeMethod(
                uiComponent, ReflectionUtils.tryToGetMethod(uiComponent.getClass(), "isDisabled")));

        return !(isReadOnly || isDisabled);
    }

    /**
     * This default implementation uses the transformed meta-data stored via the ({@link CommonMetaDataKeys#MAX_LENGTH}
     * key for initializing e.g. the maxLength attribute of the current component
     * (of type {@link HtmlInputText} or {@link HtmlInputSecret}.
     *
     * @param facesContext The JSF Context
     * @param uiComponent  The component to configure.
     * @param metaData map which contains the transformed meta-data
     */
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

            init(); //lazy init

            if(uiComponent instanceof HtmlInputText)
            {
                HtmlInputText htmlInputText = (HtmlInputText)uiComponent;

                if (this.forceComponentInitialization)
                {
                    htmlInputText.setMaxlength((Integer) maxLength);
                }
                else
                {
                    Integer initialMaxLength = (Integer)
                        htmlInputText.getAttributes().get(INITIAL_MARKUP_META_DATA_KEY);

                    if (initialMaxLength == null)
                    {
                        initialMaxLength = htmlInputText.getMaxlength(); //value overriden by the component
                        htmlInputText.getAttributes().put(INITIAL_MARKUP_META_DATA_KEY, initialMaxLength);
                    }

                    // only override maxlength if not already set by xhtml definition
                    if (initialMaxLength <= 0)
                    {
                        htmlInputText.setMaxlength((Integer) maxLength);
                    }
                }
            }
            else if(uiComponent instanceof HtmlInputSecret)
            {
                HtmlInputSecret htmlInputSecret = (HtmlInputSecret)uiComponent;

                if (this.forceComponentInitialization)
                {
                    htmlInputSecret.setMaxlength((Integer)maxLength);
                }
                else
                {
                    Integer initialMaxLength = (Integer)
                        htmlInputSecret.getAttributes().get(INITIAL_MARKUP_META_DATA_KEY);

                    if (initialMaxLength == null)
                    {
                        initialMaxLength = htmlInputSecret.getMaxlength(); //value overriden by the component
                        htmlInputSecret.getAttributes().put(INITIAL_MARKUP_META_DATA_KEY, initialMaxLength);
                    }

                    // only override maxlength if not already set by xhtml definition
                    if (initialMaxLength <= 0)
                    {
                        htmlInputSecret.setMaxlength((Integer) maxLength);
                    }
                }
            }
        }
    }

    protected void init()
    {
        if (this.forceComponentInitialization == null)
        {
            this.forceComponentInitialization = !ExtValCoreConfiguration.get().activateMarkupMetaData();
        }
    }
}
