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
package org.apache.myfaces.extensions.validator.test.trinidad.initializer;

import org.apache.myfaces.extensions.validator.PropertyValidationModuleStartupListener;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.test.trinidad.AbstractTrinidadSupportTestCase;
import org.apache.myfaces.trinidad.component.core.CoreForm;
import org.apache.myfaces.trinidad.component.core.input.CoreInputHidden;
import org.apache.myfaces.trinidad.component.core.input.CoreInputText;
import org.apache.myfaces.trinidad.component.core.output.CoreOutputLabel;
import org.junit.Assert;
import org.junit.Test;

/**
 * @author Rudy De Busscher
 */
public class TrinidadComponentInitializerTest extends AbstractTrinidadSupportTestCase
{
    private CoreInputText inputComponent;

    private CoreInputHidden inputHidden;

    private CoreOutputLabel outputLabel;

    private DataBean bean;

    @Override
    protected void invokeStartupListeners()
    {
        // We use the property validation module, so initialize it to have the strategies.
        new PropertyValidationModuleStartupListener()
        {
            private static final long serialVersionUID = 423076920926752646L;

            @Override
            protected void init()
            {
                super.initModuleConfig();
                super.init();
            }
        }.init();
        super.invokeStartupListeners();
    }


    @Override
    public void setUpTestCase()
    {


        super.setUpTestCase();

        CoreForm form = new CoreForm();
        facesContext.getViewRoot().getChildren().add(form);

        outputLabel = new CoreOutputLabel();
        //outputLabel.setParent(form);
        // For Trinidad we shouldn't set the parent our selves, it gives errors when running from SureFire.
        form.getChildren().add(outputLabel);

        inputComponent = new CoreInputText();
        inputComponent.setId("input");
        form.getChildren().add(inputComponent);

        inputHidden = new CoreInputHidden();
        inputHidden.setId("hidden");
        form.getChildren().add(inputHidden);


        bean = new DataBean();
        facesContext.getExternalContext().getRequestMap().put("testBean", bean);

    }

    @Test
    public void testInitializerStrong() throws Exception
    {
        createValueBinding(inputComponent, "value", "#{testBean.property1}");
        outputLabel.setFor("input");
        outputLabel.encodeAll(facesContext);
        Assert.assertTrue(outputLabel.isShowRequired());

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
        inputComponent.setReadOnly(true);
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
