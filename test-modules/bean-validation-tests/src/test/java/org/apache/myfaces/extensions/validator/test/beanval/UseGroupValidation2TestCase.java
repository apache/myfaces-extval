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

import org.apache.myfaces.extensions.validator.test.beanval.view.UseGroupValidationTestCase2PageBean;
import org.junit.Test;

import javax.faces.application.FacesMessage;

public class UseGroupValidation2TestCase extends BaseBeanValPropertyValidationTestCase<UseGroupValidationTestCase2PageBean>
{

    protected UseGroupValidationTestCase2PageBean getBeanToTest()
    {
        return new UseGroupValidationTestCase2PageBean();
    }

    @Test
    public void testGroup1AwareValidation()
    {
        createValueBindingForComponent(this.inputComponent1, "#{testBean.model.property1}");
        createValueBindingForComponent(this.inputComponent2, "#{testBean.model.property2}");
        setValueToValidate(this.inputComponent1, "");
        setValueToValidate(this.inputComponent2, "");

        validateComponents();

        assertComponentInvalid(this.inputComponent1);
        assertComponentValid(this.inputComponent2);

        assertNavigationBlocked(true);

        checkMessageCount(1);
        checkMessageSeverities(FacesMessage.SEVERITY_ERROR);
    }
}