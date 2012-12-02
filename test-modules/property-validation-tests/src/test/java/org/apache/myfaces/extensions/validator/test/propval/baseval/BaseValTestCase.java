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

import org.apache.myfaces.extensions.validator.test.propval.AbstractPropertyValidationTestCase;
import org.apache.myfaces.extensions.validator.test.propval.BaseValTestBean;
import org.junit.Assert;
import org.junit.Test;

/**
 */
public class BaseValTestCase extends AbstractPropertyValidationTestCase
{
    HtmlInputText inputComponent = null;

    UIViewRoot rootComponent = null;

    BaseValTestBean bean = null;

    public BaseValTestCase()
    {
        inputComponent = null;
        rootComponent = null;
        bean = null;
    }

    @SuppressWarnings({"UnusedDeclaration"})
    @Override
    public void setUpTestCase()
    {
        bean = new BaseValTestBean();
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
    public void testNameRequiredFail() throws Exception
    {
        createValueBinding(inputComponent, "value", "#{testBean.name}");

        inputComponent.setSubmittedValue("");

        inputComponent.validate(facesContext);

        Assert.assertFalse(inputComponent.isValid());
        checkMessageCount(1);
    }

    @Test
    public void testName1LenghtMaxFail() throws Exception
    {
        createValueBinding(inputComponent, "value", "#{testBean.name1}");

        //decode
        inputComponent.setSubmittedValue("12345");

        //validate
        inputComponent.validate(facesContext);

        Assert.assertFalse(inputComponent.isValid());
        checkMessageCount(1);
    }

    @Test
    public void testName1LenghtCorrect() throws Exception
    {
        createValueBinding(inputComponent, "value", "#{testBean.name1}");

        //decode
        inputComponent.setSubmittedValue("1d3");

        //validate
        inputComponent.validate(facesContext);

        Assert.assertTrue(inputComponent.isValid());
        checkMessageCount(0);
    }

    @Test
    public void testName1LenghtMinFail() throws Exception
    {
        createValueBinding(inputComponent, "value", "#{testBean.name1}");

        //decode
        inputComponent.setSubmittedValue("x");

        //validate
        inputComponent.validate(facesContext);

        Assert.assertFalse(inputComponent.isValid());
        checkMessageCount(1);
    }

    @Test
    public void testName1NoLenght() throws Exception
    {
        createValueBinding(inputComponent, "value", "#{testBean.name1}");

        //decode
        inputComponent.setSubmittedValue("");

        //validate
        inputComponent.validate(facesContext);

        Assert.assertTrue(inputComponent.isValid());
        checkMessageCount(0);
    }

    @Test
    public void testPatternNameFail() throws Exception
    {
        createValueBinding(inputComponent, "value", "#{testBean.patternName}");

        //decode
        inputComponent.setSubmittedValue("Peter1");

        //validate
        inputComponent.validate(facesContext);

        Assert.assertFalse(inputComponent.isValid());
        checkMessageCount(1);
    }

    @Test
    public void testPatternNoName() throws Exception
    {
        createValueBinding(inputComponent, "value", "#{testBean.patternName}");

        //decode
        inputComponent.setSubmittedValue("");

        //validate
        inputComponent.validate(facesContext);

        Assert.assertTrue(inputComponent.isValid());
        checkMessageCount(0);
    }

    @Test
    public void testPatternNameCorrect() throws Exception
    {
        createValueBinding(inputComponent, "value", "#{testBean.patternName}");

        //decode
        inputComponent.setSubmittedValue("Peter");

        //validate
        inputComponent.validate(facesContext);

        Assert.assertTrue(inputComponent.isValid());
        checkMessageCount(0);
    }

    @Test
    public void testDoubleValueFail() throws Exception
    {
        createValueBinding(inputComponent, "value", "#{testBean.doubleValue1}");

        //decode
        inputComponent.setSubmittedValue("-301");

        //validate
        inputComponent.validate(facesContext);

        Assert.assertFalse(inputComponent.isValid());
        checkMessageCount(1);
    }

    @Test
    public void testDoubleValueCorrect() throws Exception
    {
        createValueBinding(inputComponent, "value", "#{testBean.doubleValue1}");

        //decode
        inputComponent.setSubmittedValue("200");

        //validate
        inputComponent.validate(facesContext);

        Assert.assertTrue(inputComponent.isValid());
        checkMessageCount(0);
        Assert.assertEquals(200d, inputComponent.getValue());

        inputComponent.updateModel(facesContext);
        Assert.assertEquals(Double.valueOf(200), bean.getDoubleValue1());
    }

    @Test
    public void testDoubleNoValue() throws Exception
    {
        createValueBinding(inputComponent, "value", "#{testBean.doubleValue1}");

        //decode
        inputComponent.setSubmittedValue("");

        //validate
        inputComponent.validate(facesContext);

        Assert.assertTrue(inputComponent.isValid());
        checkMessageCount(0);
    }
}