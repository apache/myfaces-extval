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
package org.apache.myfaces.extensions.validator.test.beanval.validation;

import org.apache.myfaces.extensions.validator.test.beanval.model.ModelValidationTestCase1Bean;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class ClassLevelEqualsValidator implements
        ConstraintValidator<ClassLevelEqual, ModelValidationTestCase1Bean>
{
    public void initialize(ClassLevelEqual parameters)
    {
    }

    public boolean isValid(ModelValidationTestCase1Bean pageBean,
                           ConstraintValidatorContext constraintValidatorContext)
    {
        String property1 = pageBean.getProperty1();
        String property2 = pageBean.getProperty2();

        return property1 == null && property2 == null || property1 != null && property1.equals(property2);

    }
}
