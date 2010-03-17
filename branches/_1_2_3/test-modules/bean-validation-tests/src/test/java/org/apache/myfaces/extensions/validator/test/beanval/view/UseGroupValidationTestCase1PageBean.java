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
package org.apache.myfaces.extensions.validator.test.beanval.view;

import org.apache.myfaces.extensions.validator.test.beanval.model.GroupValidationTestCase1Bean;
import org.apache.myfaces.extensions.validator.test.beanval.validation.Group1;
import org.apache.myfaces.extensions.validator.test.beanval.validation.Group2;
import org.apache.myfaces.extensions.validator.test.beanval.validation.Group3;
import org.apache.myfaces.extensions.validator.beanval.annotation.BeanValidation;

@BeanValidation(useGroups = Group1.class)
public class UseGroupValidationTestCase1PageBean
{
    private GroupValidationTestCase1Bean model1 = new GroupValidationTestCase1Bean();

    private GroupValidationTestCase1Bean model2 = new GroupValidationTestCase1Bean();

    @BeanValidation(viewIds = {"/pages/page1.xhtml", "/pages/page2.xhtml"},useGroups = {Group2.class, Group3.class})
    private GroupValidationTestCase1Bean model3 = new GroupValidationTestCase1Bean();

    @BeanValidation.List({
        @BeanValidation(useGroups = Group2.class, conditions = "#{currentUser.adminRole}"),
        @BeanValidation(useGroups = {Group2.class, Group3.class}, conditions = "#{currentUser.userRole}")
    })
    private GroupValidationTestCase1Bean model4 = new GroupValidationTestCase1Bean();

    public GroupValidationTestCase1Bean getModel1()
    {
        return model1;
    }

    @BeanValidation(useGroups = Group2.class)
    public GroupValidationTestCase1Bean getModel2()
    {
        return model2;
    }

    public GroupValidationTestCase1Bean getModel3()
    {
        return model3;
    }

    public GroupValidationTestCase1Bean getModel4()
    {
        return model4;
    }
}