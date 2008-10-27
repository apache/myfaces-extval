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
package org.apache.myfaces.extensions.validator.baseval;

import javax.faces.component.UIViewRoot;
import javax.faces.component.html.HtmlForm;
import javax.faces.component.html.HtmlInputText;
import javax.faces.el.ValueBinding;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.apache.myfaces.extensions.validator.AbstractExValViewControllerTestCase;

/**
 * @author Leonardo Uribe
 */
public class BaseValTestCase extends AbstractExValViewControllerTestCase
{

    HtmlInputText inputComponent = null;

    UIViewRoot rootComponent = null;
    
    BaseValTestBean bean = null;

    public BaseValTestCase(String name)
    {
        super(name);
        inputComponent = null;
        rootComponent = null;
        bean = null;
    }

    public static Test suite()
    {
        return new TestSuite(BaseValTestCase.class);
    }

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();
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
    protected void tearDown() throws Exception
    {
        super.tearDown();
        inputComponent = null;
        rootComponent = null;
        bean = null;        
    }

    public void testNameRequiredFail() throws Exception
    {
        inputComponent.setValueBinding("value", application
                .createValueBinding("#{testBean.name}"));

        inputComponent.setSubmittedValue("");

        inputComponent.validate(facesContext);

        assertFalse(inputComponent.isValid());
        checkMessageCount(1);
    }

    public void testName1LenghtMaxFail() throws Exception
    {
        inputComponent.setValueBinding("value", application
                .createValueBinding("#{testBean.name1}"));

        //decode
        inputComponent.setSubmittedValue("12345");

        //validate
        inputComponent.validate(facesContext);

        assertFalse(inputComponent.isValid());
        checkMessageCount(1);
    }

    public void testName1LenghtCorrect() throws Exception
    {
        inputComponent.setValueBinding("value", application
                .createValueBinding("#{testBean.name1}"));

        //decode
        inputComponent.setSubmittedValue("1d3");

        //validate
        inputComponent.validate(facesContext);

        assertTrue(inputComponent.isValid());
        checkMessageCount(0);
    }

    public void testName1LenghtMinFail() throws Exception
    {
        inputComponent.setValueBinding("value", application
                .createValueBinding("#{testBean.name1}"));

        //decode
        inputComponent.setSubmittedValue("x");

        //validate
        inputComponent.validate(facesContext);

        assertFalse(inputComponent.isValid());
        checkMessageCount(1);
    }
    
    public void testPatternNameFail() throws Exception
    {
        inputComponent.setValueBinding("value", application
                .createValueBinding("#{testBean.patternName}"));

        //decode
        inputComponent.setSubmittedValue("Peter1");

        //validate
        inputComponent.validate(facesContext);

        assertFalse(inputComponent.isValid());
        checkMessageCount(1);
    }
    
    public void testPatternNameCorrect() throws Exception
    {
        inputComponent.setValueBinding("value", application
                .createValueBinding("#{testBean.patternName}"));

        //decode
        inputComponent.setSubmittedValue("Peter");

        //validate
        inputComponent.validate(facesContext);

        assertTrue(inputComponent.isValid());
        checkMessageCount(0);
    }    

    public void testDoubleValueFail() throws Exception
    {
        inputComponent.setValueBinding("value", application
                .createValueBinding("#{testBean.doubleValue1}"));

        //decode
        inputComponent.setSubmittedValue("-301");

        //validate
        inputComponent.validate(facesContext);

        assertFalse(inputComponent.isValid());
        checkMessageCount(1);
    }

    public void testDoubleValueCorrect() throws Exception
    {
        inputComponent.setValueBinding("value", application
                .createValueBinding("#{testBean.doubleValue1}"));

        //decode
        inputComponent.setSubmittedValue("200");

        //validate
        inputComponent.validate(facesContext);

        assertTrue(inputComponent.isValid());
        checkMessageCount(0);
        assertEquals(200d, inputComponent.getValue());
        
        inputComponent.updateModel(facesContext);
        assertEquals(200d, bean.getDoubleValue1());
    }    
}
