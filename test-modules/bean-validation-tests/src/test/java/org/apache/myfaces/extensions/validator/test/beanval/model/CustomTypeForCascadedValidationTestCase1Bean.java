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
package org.apache.myfaces.extensions.validator.test.beanval.model;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

public class CustomTypeForCascadedValidationTestCase1Bean
{
    @NotNull
    private String property1;

    @Size(min = 3)
    private String property2;

    public CustomTypeForCascadedValidationTestCase1Bean(String property1, String property2)
    {
        this.property1 = property1;
        this.property2 = property2;
    }

    public String getProperty1()
    {
        return property1;
    }

    public String getProperty2()
    {
        return property2;
    }
}
