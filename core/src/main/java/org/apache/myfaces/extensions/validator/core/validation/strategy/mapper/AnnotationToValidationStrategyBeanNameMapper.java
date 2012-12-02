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
package org.apache.myfaces.extensions.validator.core.validation.strategy.mapper;

import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.InvocationOrder;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.beans.Introspector;

/**
 * Name Mapper which delegates the name mapping, extract the name and convert it to a bean name + prefix
 * target: configure a validation strategy via a managed bean facility -> allows to inject other beans
 * instead of api calls + hardcoded bean names
 * <p/>
 * allowed bean scopes:
 * the validation strategy is stateless: application/singleton
 * the validation strategy is stateful: none/prototype
 * don't use the session or a conversation scope
 *
 * @since 1.x.1
 */
@InvocationOrder(500)
@UsageInformation(UsageCategory.INTERNAL)
public class AnnotationToValidationStrategyBeanNameMapper extends AbstractMetaDataToValidationStrategyNameMapper
{
    public static final String PREFIX_FOR_BEAN_MAPPING = "bean:";
    private NameMapper<String> wrapped;

    public AnnotationToValidationStrategyBeanNameMapper(NameMapper<String> nameMapper)
    {
        this.wrapped = nameMapper;
    }

    public String createName(String source)
    {
        String name = wrapped.createName(source);

        if (name == null)
        {
            return null;
        }

        name = name.substring(name.lastIndexOf(".") + 1);
        return PREFIX_FOR_BEAN_MAPPING + Introspector.decapitalize(name);
    }
}
