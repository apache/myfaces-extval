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
package org.apache.myfaces.extensions.validator.test.beanval;

import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.interceptor.SingleViolationPropertyValidationInterceptor;

import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlForm;
import javax.faces.component.UIViewRoot;
import javax.faces.component.UIInput;
import javax.faces.el.ValueBinding;

public abstract class BaseBeanValPropertyValidationTestCase<T> extends AbstractBeanValidationTestCase
{
    public BaseBeanValPropertyValidationTestCase(String name)
    {
        super(name);
        inputComponent1 = null;
        inputComponent2 = null;
        inputComponent3 = null;
        rootComponent = null;
        bean = null;
    }

    protected HtmlInputText inputComponent1 = null;
    protected HtmlInputText inputComponent2 = null;
    protected HtmlInputText inputComponent3 = null;

    private UIViewRoot rootComponent = null;

    protected T bean;

    @SuppressWarnings({"UnusedDeclaration"})
    @Override
    protected void setUp() throws Exception
    {
        super.setUp();
        bean = getBeanToTest();
        bindBeanToExpression();

        createComponents();

        ExtValContext.getContext().addPropertyValidationInterceptor(new SingleViolationPropertyValidationInterceptor());
    }

    private void createComponents()
    {
        rootComponent = new UIViewRoot();
        HtmlForm form = new HtmlForm();
        form.setId("form");
        rootComponent.getChildren().add(form);
        inputComponent1 = new HtmlInputText();
        inputComponent2 = new HtmlInputText();
        inputComponent3 = new HtmlInputText();
        form.getChildren().add(inputComponent1);
        form.getChildren().add(inputComponent2);
        form.getChildren().add(inputComponent3);
        inputComponent1.setId("input1");
        inputComponent2.setId("input2");
        inputComponent3.setId("input3");
    }

    @SuppressWarnings({"UnusedDeclaration"})
    private void bindBeanToExpression()
    {
        ValueBinding vb = application.createValueBinding("#{testBean}");
        facesContext.getExternalContext().getRequestMap().put("testBean", bean);
    }

    protected abstract T getBeanToTest();

    @Override
    protected void tearDown() throws Exception
    {
        super.tearDown();
        inputComponent1 = null;
        inputComponent2 = null;
        inputComponent3 = null;
        rootComponent = null;
        bean = null;
    }

    protected void createValueBindingForComponent(UIInput uiComponent, String valueBinding)
    {
        createValueBinding(uiComponent, "value", valueBinding);
    }

    protected void setValueToValidate(UIInput uiComponent, String valueToValidate)
    {
        uiComponent.setSubmittedValue(valueToValidate);
    }

    protected void validateComponents()
    {
        inputComponent1.processValidators(facesContext);
        inputComponent2.processValidators(facesContext);
        inputComponent3.processValidators(facesContext);
    }

    protected void updateComponents()
    {
        inputComponent1.processUpdates(facesContext);
        inputComponent2.processUpdates(facesContext);
        inputComponent3.processUpdates(facesContext);
    }

    protected void assertComponentValid(UIInput uiInput)
    {
        assertTrue(isComponentValid(uiInput));
    }

    protected void assertComponentInvalid(UIInput uiInput)
    {
        assertFalse(isComponentValid(uiInput));
    }

    private boolean isComponentValid(UIInput uiComponent)
    {
        return uiComponent.isValid();
    }
}
