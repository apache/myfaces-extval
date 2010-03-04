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
package org.apache.myfaces.extensions.validator.test.propval.crossval;

import junit.framework.Test;
import junit.framework.TestSuite;
import org.apache.myfaces.extensions.validator.test.propval.AbstractPropertyValidationTestCase;

import javax.faces.component.UIViewRoot;
import javax.faces.component.html.HtmlForm;
import javax.faces.component.html.HtmlInputText;
import javax.faces.application.FacesMessage;

public class RequiredIf3TestCase extends AbstractPropertyValidationTestCase
{

    HtmlInputText inputComponent1 = null;
    HtmlInputText inputComponent2 = null;

    UIViewRoot rootComponent = null;

    public RequiredIf3TestCase(String name)
    {
        super(name);
        inputComponent1 = null;
        inputComponent2 = null;
        rootComponent = null;
    }

    public static Test suite()
    {
        return new TestSuite(RequiredIf3TestCase.class);
    }

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();
        RequiredIf3TestBean bean = new RequiredIf3TestBean();
        createValueBinding(null, "value", "#{testBean}");
        facesContext.getExternalContext().getRequestMap().put("testBean", bean);

        rootComponent = new UIViewRoot();
        HtmlForm form = new HtmlForm();
        form.setId("form");
        rootComponent.getChildren().add(form);
        inputComponent1 = new HtmlInputText();
        form.getChildren().add(inputComponent1);
        inputComponent1.setId("input1");
        inputComponent2 = new HtmlInputText();
        form.getChildren().add(inputComponent2);
        inputComponent2.setId("input2");
    }

    @Override
    protected void tearDown() throws Exception
    {
        super.tearDown();
    }

    public void testRequiredIfBooleanTargetNotEmptyFailed() throws Exception
    {
        createValueBinding(inputComponent1, "value", "#{testBean.property1}");
        createValueBinding(inputComponent2, "value", "#{testBean.property2}");

        //decode
        inputComponent1.setSubmittedValue(Boolean.TRUE.toString());
        inputComponent2.setSubmittedValue("");

        //validate
        inputComponent1.validate(facesContext);
        inputComponent2.validate(facesContext);

        processCrossValidation();
        checkMessageCount(1);

        assertNavigationBlocked(true);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);

        //no update model needed
    }

    public void testRequiredIfBooleanTargetNotEmptyCorrect1() throws Exception
    {
        createValueBinding(inputComponent1, "value", "#{testBean.property1}");
        createValueBinding(inputComponent2, "value", "#{testBean.property2}");

        //decode
        inputComponent1.setSubmittedValue(Boolean.TRUE.toString());
        inputComponent2.setSubmittedValue("1d3");

        //validate
        inputComponent1.validate(facesContext);
        inputComponent2.validate(facesContext);

        processCrossValidation();
        checkMessageCount(0);

        assertNavigationBlocked(false);

        //no update model needed
    }

    public void testRequiredIfBooleanTargetNotEmptyCorrect2() throws Exception
    {
        createValueBinding(inputComponent1, "value", "#{testBean.property1}");
        createValueBinding(inputComponent2, "value", "#{testBean.property2}");

        //decode
        inputComponent1.setSubmittedValue(Boolean.FALSE.toString());
        inputComponent2.setSubmittedValue("");

        //validate
        inputComponent1.validate(facesContext);
        inputComponent2.validate(facesContext);

        processCrossValidation();
        checkMessageCount(0);

        assertNavigationBlocked(false);

        //no update model needed
    }

    public void testRequiredIfBooleanTargetNotEmptyCorrect3() throws Exception
    {
        createValueBinding(inputComponent1, "value", "#{testBean.property1}");
        createValueBinding(inputComponent2, "value", "#{testBean.property2}");

        //decode
        inputComponent1.setSubmittedValue(Boolean.FALSE.toString());
        inputComponent2.setSubmittedValue("1d3");

        //validate
        inputComponent1.validate(facesContext);
        inputComponent2.validate(facesContext);

        processCrossValidation();
        checkMessageCount(0);

        assertNavigationBlocked(false);

        //no update model needed
    }
}