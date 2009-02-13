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
package org.apache.myfaces.extensions.validator.crossval;

import junit.framework.Test;
import junit.framework.TestSuite;
import org.apache.myfaces.extensions.validator.AbstractExValViewControllerTestCase;

import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlForm;
import javax.faces.component.UIViewRoot;
import javax.faces.convert.DateTimeConverter;
import java.util.Date;

/**
 * @author Gerhard Petracek
 */
public class PropertyChainCrossValTestCase extends AbstractExValViewControllerTestCase
{
    HtmlInputText inputComponent1 = null;
    HtmlInputText inputComponent2 = null;

    UIViewRoot rootComponent = null;

    public static Test suite()
    {
        return new TestSuite(PropertyChainCrossValTestCase.class);
    }

    public PropertyChainCrossValTestCase(String name)
    {
        super(name);
        inputComponent1 = null;
        inputComponent2 = null;
        rootComponent = null;
    }

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();
        createRequestScopedBean("bean1", getEntityInstance());

        rootComponent = new UIViewRoot();
        HtmlForm form = new HtmlForm();
        form.setId("form");
        rootComponent.getChildren().add(form);

        //input1
        inputComponent1 = new HtmlInputText();
        inputComponent1.setId("input1");
        inputComponent1.setConverter(new DateTimeConverter());
        ((DateTimeConverter)inputComponent1.getConverter()).setPattern("dd.MM.yyyy");
        form.getChildren().add(inputComponent1);

        //input2
        inputComponent2 = new HtmlInputText();
        inputComponent2.setId("input2");
        inputComponent2.setConverter(new DateTimeConverter());
        ((DateTimeConverter)inputComponent2.getConverter()).setPattern("dd.MM.yyyy");
        form.getChildren().add(inputComponent2);
    }

    @Override
    protected void tearDown() throws Exception
    {
        super.tearDown();
    }

    public void testCrossComponentEqualsValidationCorrect() throws Exception
    {
        validatePropertyChainCrossComponentValidationUseCase("14.05.1983", "14.05.1983");

        checkMessageCount(0);
    }

    public void testCrossComponentEqualsValidationFailedValidation() throws Exception
    {
        validatePropertyChainCrossComponentValidationUseCase("14.05.1983", "12.12.2008");

        checkMessageCount(2);
    }

    private void validatePropertyChainCrossComponentValidationUseCase(String valueBean1Property1, String valueBean2Property2)
    {
        createValueBinding(inputComponent1, "value", "#{bean1.date1}");
        createValueBinding(inputComponent2, "value", "#{bean1.subBean.date2}");

        //decode
        inputComponent1.setSubmittedValue(valueBean1Property1);
        inputComponent2.setSubmittedValue(valueBean2Property2);

        //validate
        inputComponent1.validate(facesContext);
        inputComponent2.validate(facesContext);

        processCrossValValidation();

        //no update model needed
    }

    public void testModelAwareCrossEqualsValidationCorrect() throws Exception
    {
        validateELModelAwareCrossValidationUseCase("14.05.1983", "14.05.1983");

        checkMessageCount(0);
    }

    public void testModelAwareCrossEqualsValidationFailedValidation() throws Exception
    {
        validateELModelAwareCrossValidationUseCase("14.05.1983", "12.12.2008");

        checkMessageCount(1);
    }

    private void validateELModelAwareCrossValidationUseCase(String valueBean1Property1, String valueBean2Property1)
    {
        createValueBinding(inputComponent1, "value", "#{bean1.date1}");

        //set model values
        resolveBean("bean1", getEntityInstance().getClass()).getSubBean().setDate2((Date)inputComponent2.getConverter().getAsObject(facesContext, inputComponent2, valueBean2Property1));


        //decode
        inputComponent1.setSubmittedValue(valueBean1Property1);

        //validate
        inputComponent1.validate(facesContext);

        processCrossValValidation();

        //no update model needed
    }

    protected PropertyChainCrossValTestDateBean getEntityInstance()
    {
        return new PropertyChainCrossValTestDateBean();
    }
}
