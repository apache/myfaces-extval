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

import org.apache.myfaces.extensions.validator.test.beanval.view.ModelValidationTestCase1PageBean;
import org.apache.myfaces.extensions.validator.test.beanval.model.SimulatedUserInformation;
import org.junit.Test;


import javax.faces.application.FacesMessage;

public class ModelValidation1TestCase extends
        BaseBeanValPropertyValidationTestCase<ModelValidationTestCase1PageBean>
{

    protected ModelValidationTestCase1PageBean getBeanToTest()
    {
        return new ModelValidationTestCase1PageBean();
    }

    @Test
    public void testModelValidation()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.model1.property1}");
        setValueToValidate(this.inputComponent1, "123");

        createValueBindingForComponent(this.inputComponent2, "#{testBean.model1.property2}");
        setValueToValidate(this.inputComponent2, "123");

        validateComponents();
        updateComponents();
        processModelValidation();

        assertNavigationBlocked(false);

        checkMessageCount(0);
    }

    @Test
    public void testModelViolationWithGlobalViolationMessageViaProperty()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.model1.property1}");
        setValueToValidate(this.inputComponent1, "123");

        createValueBindingForComponent(this.inputComponent2, "#{testBean.model1.property2}");
        setValueToValidate(this.inputComponent2, "456");

        validateComponents();
        updateComponents();
        processModelValidation();

        assertNavigationBlocked(true);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);
    }

    @Test
    public void testModelViolationWithInlineViolationMessageViaProperty()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.model2.property1}");
        setValueToValidate(this.inputComponent1, "123");

        createValueBindingForComponent(this.inputComponent2, "#{testBean.model2.property2}");
        setValueToValidate(this.inputComponent2, "456");

        validateComponents();
        updateComponents();
        processModelValidation();

        assertComponentInvalid(this.inputComponent1);
        assertComponentInvalid(this.inputComponent2);
        assertNavigationBlocked(true);

        //due to displayMessageInline
        checkMessageCount(2);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR, FacesMessage.SEVERITY_ERROR);
    }

    @Test
    public void testModelViolationWithGlobalViolationMessageViaField()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.model3.property1}");
        setValueToValidate(this.inputComponent1, "123");

        createValueBindingForComponent(this.inputComponent2, "#{testBean.model3.property2}");
        setValueToValidate(this.inputComponent2, "456");

        validateComponents();
        updateComponents();
        processModelValidation();

        assertNavigationBlocked(true);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);
    }

    @Test
    public void testModelViolationWithInlineViolationMessageViaField()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.model4.property1}");
        setValueToValidate(this.inputComponent1, "123");

        createValueBindingForComponent(this.inputComponent2, "#{testBean.model4.property2}");
        setValueToValidate(this.inputComponent2, "456");

        validateComponents();
        updateComponents();
        processModelValidation();

        assertComponentInvalid(this.inputComponent1);
        assertComponentInvalid(this.inputComponent2);
        assertNavigationBlocked(true);

        //due to displayMessageInline
        checkMessageCount(2);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR, FacesMessage.SEVERITY_ERROR);
    }

    @Test
    public void testModelViolationWithAdminRole()
    {
        createRequestScopedBean("currentUser", new SimulatedUserInformation("admin"));

        createValueBindingForComponent(this.inputComponent1, "#{testBean.model5.property1}");
        setValueToValidate(this.inputComponent1, "123");

        createValueBindingForComponent(this.inputComponent2, "#{testBean.model5.property2}");
        setValueToValidate(this.inputComponent2, "456");

        validateComponents();
        updateComponents();
        processModelValidation();

        assertNavigationBlocked(false);

        checkMessageCount(0);
    }

    @Test
    public void testModelViolationWithUserRole()
    {
        createRequestScopedBean("currentUser", new SimulatedUserInformation("user"));

        createValueBindingForComponent(this.inputComponent1, "#{testBean.model5.property1}");
        setValueToValidate(this.inputComponent1, "123");

        createValueBindingForComponent(this.inputComponent2, "#{testBean.model5.property2}");
        setValueToValidate(this.inputComponent2, "456");

        validateComponents();
        updateComponents();
        processModelValidation();

        assertNavigationBlocked(true);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);
    }
}