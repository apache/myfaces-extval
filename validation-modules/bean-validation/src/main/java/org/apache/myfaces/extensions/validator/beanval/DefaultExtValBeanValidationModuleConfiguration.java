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
package org.apache.myfaces.extensions.validator.beanval;

import org.apache.myfaces.extensions.validator.core.ExtValContext;

import javax.validation.ValidatorFactory;

/**
 */
public class DefaultExtValBeanValidationModuleConfiguration extends ExtValBeanValidationModuleConfiguration
{
    public ValidatorFactory customValidatorFactory()
    {
        Object factory = ExtValContext.getContext().getGlobalProperty(ValidatorFactory.class.getName());

        if(factory instanceof ValidatorFactory)
        {
            return (ValidatorFactory)factory;
        }

        return null;
    }

    @Override
    public ExtValBeanValidationContext customExtValBeanValidationContext()
    {
        Object context = ExtValContext.getContext().getGlobalProperty(ExtValBeanValidationContext.class.getName());

        if(context instanceof ExtValBeanValidationContext)
        {
            return (ExtValBeanValidationContext)context;
        }

        return null;
    }
}
