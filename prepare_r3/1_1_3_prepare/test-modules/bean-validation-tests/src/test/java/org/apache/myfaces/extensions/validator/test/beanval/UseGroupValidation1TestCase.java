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

import junit.framework.Test;
import junit.framework.TestSuite;
import org.apache.myfaces.extensions.validator.test.beanval.view.UseGroupValidationTestCase1PageBean;
import org.apache.myfaces.extensions.validator.test.beanval.model.SimulatedUserInformation;

import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;

public class UseGroupValidation1TestCase extends BaseBeanValPropertyValidationTestCase<UseGroupValidationTestCase1PageBean>
{
    public UseGroupValidation1TestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(UseGroupValidation1TestCase.class);
    }

    protected UseGroupValidationTestCase1PageBean getBeanToTest()
    {
        return new UseGroupValidationTestCase1PageBean();
    }

    public void testGroup1AwareValidation()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.model1.property1}");
        createValueBindingForComponent(this.inputComponent2, "#{testBean.model1.property2}");
        setValueToValidate(this.inputComponent1, "");
        setValueToValidate(this.inputComponent2, "");

        validateComponents();

        assertComponentInvalid(this.inputComponent1);
        assertComponentValid(this.inputComponent2);
        assertNavigationBlocked(true);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);
    }

    public void testGroup1AndGroup2AwareValidation()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.model2.property1}");
        createValueBindingForComponent(this.inputComponent2, "#{testBean.model2.property2}");
        setValueToValidate(this.inputComponent1, "");
        setValueToValidate(this.inputComponent2, "");

        validateComponents();

        assertComponentInvalid(this.inputComponent1);
        assertComponentInvalid(this.inputComponent2);
        assertNavigationBlocked(true);

        checkMessageCount(2);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR, FacesMessage.SEVERITY_ERROR);
    }

    public void testGroup2AndGroup3AwareValidationWithWrongViewId()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.model3.property1}");
        createValueBindingForComponent(this.inputComponent2, "#{testBean.model3.property2}");
        setValueToValidate(this.inputComponent1, "g");
        setValueToValidate(this.inputComponent2, "p");

        validateComponents();

        assertComponentValid(this.inputComponent1);
        assertComponentValid(this.inputComponent2);
        assertNavigationBlocked(false);

        checkMessageCount(0);
    }

    public void testGroup2AndGroup3AwareValidationWithCorrectViewId1()
    {
        FacesContext.getCurrentInstance().getViewRoot().setViewId("/pages/page1.xhtml");
        createValueBindingForComponent(this.inputComponent1, "#{testBean.model3.property1}");
        createValueBindingForComponent(this.inputComponent2, "#{testBean.model3.property2}");
        setValueToValidate(this.inputComponent1, "g");
        setValueToValidate(this.inputComponent2, "p");

        validateComponents();

        assertComponentValid(this.inputComponent1);
        assertComponentInvalid(this.inputComponent2);
        assertNavigationBlocked(true);

        checkMessageCount(2);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR, FacesMessage.SEVERITY_ERROR);
    }

    public void testGroup2AndGroup3AwareValidationWithCorrectViewId2()
    {
        FacesContext.getCurrentInstance().getViewRoot().setViewId("/pages/page2.xhtml");
        createValueBindingForComponent(this.inputComponent1, "#{testBean.model3.property1}");
        createValueBindingForComponent(this.inputComponent2, "#{testBean.model3.property2}");
        setValueToValidate(this.inputComponent1, "g");
        setValueToValidate(this.inputComponent2, "p");

        validateComponents();

        assertComponentValid(this.inputComponent1);
        assertComponentInvalid(this.inputComponent2);
        assertNavigationBlocked(true);

        checkMessageCount(2);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR, FacesMessage.SEVERITY_ERROR);
    }

    public void testGroup2AwareValidationWithRoleAdmin()
    {
        createRequestScopedBean("currentUser", new SimulatedUserInformation("admin"));
        createValueBindingForComponent(this.inputComponent1, "#{testBean.model4.property1}");
        createValueBindingForComponent(this.inputComponent2, "#{testBean.model4.property2}");
        setValueToValidate(this.inputComponent1, "g"); //don't force a violation with Group1
        setValueToValidate(this.inputComponent2, "");

        validateComponents();

        assertComponentValid(this.inputComponent1);
        assertComponentInvalid(this.inputComponent2);
        assertNavigationBlocked(true);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);
    }

    public void testGroup2AndGroup3AwareValidationWithRoleUser()
    {
        createRequestScopedBean("currentUser", new SimulatedUserInformation("user"));
        createValueBindingForComponent(this.inputComponent1, "#{testBean.model4.property1}");
        createValueBindingForComponent(this.inputComponent2, "#{testBean.model4.property2}");
        setValueToValidate(this.inputComponent1, "g");
        setValueToValidate(this.inputComponent2, "p");

        validateComponents();

        assertComponentValid(this.inputComponent1);
        assertComponentInvalid(this.inputComponent2);
        assertNavigationBlocked(true);

        checkMessageCount(2);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR, FacesMessage.SEVERITY_ERROR);
    }
}