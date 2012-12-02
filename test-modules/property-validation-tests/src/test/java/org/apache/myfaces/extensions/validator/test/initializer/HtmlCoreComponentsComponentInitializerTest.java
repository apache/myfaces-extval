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
package org.apache.myfaces.extensions.validator.test.initializer;

import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.test.propval.AbstractPropertyValidationTestCase;
import org.junit.Assert;
import org.junit.Test;

import javax.faces.component.html.HtmlForm;
import javax.faces.component.html.HtmlInputHidden;
import javax.faces.component.html.HtmlInputText;

/**
 * Although this is mainly a test for AbstractHtmlCoreComponentsComponentInitializer, it is in the property validation modules test
 * since we need a few classes that are available here to make a meaningful test.
 *
 */
public class HtmlCoreComponentsComponentInitializerTest extends AbstractPropertyValidationTestCase
{

    private HtmlInputText inputComponent;

    private HtmlInputHidden inputHidden;

    private DataBean bean;

    @Override
    public void setUpTestCase()
    {
        DefaultExtValCoreConfiguration.overruleActivateRequiredInitialization(true, true);

        super.setUpTestCase();

        HtmlForm form = new HtmlForm();
        facesContext.getViewRoot().getChildren().add(form);

        inputComponent = new HtmlInputText();
        inputComponent.setParent(form);
        form.getChildren().add(inputComponent);

        inputHidden = new HtmlInputHidden();
        inputHidden.setParent(form);
        form.getChildren().add(inputHidden);

        bean = new DataBean();
        facesContext.getExternalContext().getRequestMap().put("testBean", bean);

    }

    @Test
    public void testInitializerStrong() throws Exception
    {
        createValueBinding(inputComponent, "value", "#{testBean.property1}");
        inputComponent.encodeAll(facesContext);
        Assert.assertTrue(inputComponent.isRequired());
    }

    @Test
    public void testInitializerRequiredIf() throws Exception
    {
        bean.setProperty1("X");
        createValueBinding(inputComponent, "value", "#{testBean.property2}");
        inputComponent.encodeAll(facesContext);
        // although at rendering the property1 has a value, it doesn't make it required.
        // Because at postback, the field could be empty and thus property2 is not required anymore.
        Assert.assertFalse(inputComponent.isRequired());
    }

    @Test
    public void testInitializerWeak() throws Exception
    {
        createValueBinding(inputComponent, "value", "#{testBean.property3}");
        inputComponent.encodeAll(facesContext);
        Assert.assertTrue(inputComponent.isRequired());
    }

    @Test
    public void testInitializerNone() throws Exception
    {
        createValueBinding(inputComponent, "value", "#{testBean.property4}");
        inputComponent.encodeAll(facesContext);
        Assert.assertFalse(inputComponent.isRequired());
    }

    @Test
    public void testInitializerReadOnly() throws Exception
    {
        createValueBinding(inputComponent, "value", "#{testBean.property1}");
        inputComponent.setReadonly(true);
        inputComponent.encodeAll(facesContext);
        Assert.assertFalse(inputComponent.isRequired());
    }

    @Test
    public void testInitializerDisabled() throws Exception
    {
        createValueBinding(inputComponent, "value", "#{testBean.property1}");
        inputComponent.setDisabled(true);
        inputComponent.encodeAll(facesContext);
        Assert.assertFalse(inputComponent.isRequired());
    }

    @Test
    public void testInitializerInputHidden() throws Exception
    {
        createValueBinding(inputHidden, "value", "#{testBean.property1}");
        inputComponent.encodeAll(facesContext);
        Assert.assertFalse(inputComponent.isRequired());
    }

    @Test
    public void testInitializerNotActivated() throws Exception
    {

        // Here we override it again so that we are back in the defult state of not handling the initialization.
        DefaultExtValCoreConfiguration.overruleActivateRequiredInitialization(false, true);
        createValueBinding(inputComponent, "value", "#{testBean.property1}");
        inputComponent.encodeAll(facesContext);
        Assert.assertFalse(inputComponent.isRequired());
    }

    @Test
    public void testInitializerNoValidationOfEmptyFields() throws Exception
    {

        // Use temporary a version of the config that specifies that empty fields shouldn't be validated.
        ExtValCoreConfiguration currentModuleConfig = ExtValCoreConfiguration.get();
        ExtValCoreConfiguration.use(new DefaultExtValCoreConfiguration()
        {
            @Override
            public boolean validateEmptyFields()
            {
                return false;
            }
        }, true);

        createValueBinding(inputComponent, "value", "#{testBean.property1}");
        inputComponent.encodeAll(facesContext);
        Assert.assertFalse(inputComponent.isRequired());

        // set old initialization back for other test cases.
        ExtValCoreConfiguration.use(currentModuleConfig, true);


    }

}
