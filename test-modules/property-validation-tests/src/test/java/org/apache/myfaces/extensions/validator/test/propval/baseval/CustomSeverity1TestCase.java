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
package org.apache.myfaces.extensions.validator.test.propval.baseval;


import javax.faces.component.UIViewRoot;
import javax.faces.component.html.HtmlForm;
import javax.faces.component.html.HtmlInputText;
import javax.faces.el.ValueBinding;
import javax.faces.application.FacesMessage;

import org.apache.myfaces.extensions.validator.test.propval.AbstractPropertyValidationTestCase;
import org.apache.myfaces.extensions.validator.test.propval.CustomSeverityTestBean;
import org.apache.myfaces.extensions.validator.test.propval.custom.CustomViolationSeverity;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticInMemoryConfiguration;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationNames;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ViolationSeverity;
import org.junit.Assert;
import org.junit.Test;

public class CustomSeverity1TestCase extends AbstractPropertyValidationTestCase
{
    private HtmlInputText inputComponent = null;

    private UIViewRoot rootComponent = null;

    private CustomSeverityTestBean bean = null;

    public CustomSeverity1TestCase()
    {
        inputComponent = null;
        rootComponent = null;
        bean = null;
    }

    @SuppressWarnings({"UnusedDeclaration"})
    @Override
    public void setUpTestCase()
    {
            bean = new CustomSeverityTestBean();
        ValueBinding vb = application.createValueBinding("#{testBean}");
        facesContext.getExternalContext().getRequestMap().put("testBean", bean);

        rootComponent = new UIViewRoot();
        HtmlForm form = new HtmlForm();
        form.setId("form");
        rootComponent.getChildren().add(form);
        inputComponent = new HtmlInputText();
        form.getChildren().add(inputComponent);
        inputComponent.setId("input1");
    }

    @Override
    protected void resetTestCase()
    {
        super.resetTestCase();
        inputComponent = null;
        rootComponent = null;
        bean = null;
    }

    @Test
    public void testCustomValidationParameterViaGlobalProperty() throws Exception
    {
        ExtValContext.getContext()
                .addGlobalProperty(ViolationSeverity.class.getName(), CustomViolationSeverity.class);

        createValueBinding(inputComponent, "value", "#{testBean.name}");

        inputComponent.setSubmittedValue("");

        inputComponent.validate(facesContext);

        Assert.assertTrue(inputComponent.isValid());

        assertNavigationBlocked(false);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_WARN);
    }

    @Test
    public void testCustomValidationParameterViaStaticConfig() throws Exception
    {
        StaticInMemoryConfiguration config = new StaticInMemoryConfiguration();
        config.addMapping(ViolationSeverity.class.getName(), CustomViolationSeverity.class.getName());

        ExtValContext.getContext()
                .addStaticConfiguration(StaticConfigurationNames.VALIDATION_PARAMETER_CONFIG, config);

        createValueBinding(inputComponent, "value", "#{testBean.name}");

        inputComponent.setSubmittedValue("");

        inputComponent.validate(facesContext);

        Assert.assertTrue(inputComponent.isValid());

        assertNavigationBlocked(false);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_WARN);
    }

    @Test
    public void testUnknownValidationParameter() throws Exception
    {
        createValueBinding(inputComponent, "value", "#{testBean.name}");

        inputComponent.setSubmittedValue("");

        inputComponent.validate(facesContext);

        Assert.assertFalse(inputComponent.isValid());

        assertNavigationBlocked(true);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);
    }
}