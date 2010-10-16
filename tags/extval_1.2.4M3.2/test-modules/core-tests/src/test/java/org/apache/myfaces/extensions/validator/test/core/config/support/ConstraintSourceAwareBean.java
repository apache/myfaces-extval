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
package org.apache.myfaces.extensions.validator.test.core.config.support;

import org.apache.myfaces.extensions.validator.core.validation.ConstraintSource;
import org.apache.myfaces.extensions.validator.core.validation.IgnoreConstraintSource;
import org.apache.myfaces.extensions.validator.core.validation.TargetProperty;
import org.apache.myfaces.extensions.validator.core.validation.TargetPropertyId;

@ConstraintSource(ConstraintSourceAwareMetaDataBean.class)
public class ConstraintSourceAwareBean
{
    @IgnoreConstraintSource
    private String property1;

    @CustomIgnoreConstraintSource
    private String property2;

	@TargetProperty(value="test1")
    private String property3;

	@CustomTargetProperty(value="test2")
    private String property4;

	@TargetPropertyId(value=ConstraintSource.class)
    private String property5;

	@CustomTargetPropertyId(value=CustomConstraintSource.class)
    private String property6;

    public String getProperty1()
    {
        return property1;
    }

    public void setProperty1(String property1)
    {
        this.property1 = property1;
    }

    @CustomConstraintSource(ConstraintSourceAware2MetaDataBean.class)
    public String getProperty2()
    {
        return property2;
    }

    public void setProperty2(String property2)
    {
        this.property2 = property2;
    }
}


