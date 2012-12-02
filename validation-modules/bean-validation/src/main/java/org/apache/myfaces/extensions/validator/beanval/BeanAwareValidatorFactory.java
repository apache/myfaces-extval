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

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.validation.ValidatorFactory;
import javax.validation.Validator;
import javax.validation.ValidatorContext;
import javax.validation.MessageInterpolator;
import javax.validation.TraversableResolver;
import javax.validation.ConstraintValidatorFactory;

/**
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class BeanAwareValidatorFactory implements ValidatorFactory
{
    private ValidatorFactory validatorFactory;

    public BeanAwareValidatorFactory(ValidatorFactory validatorFactory)
    {
        setValidatorFactory(validatorFactory);
    }

    public void setValidatorFactory(ValidatorFactory validatorFactory)
    {
        if(validatorFactory == null)
        {
            throw new IllegalStateException("null is not allowed here");
        }
        this.validatorFactory = validatorFactory;
    }

    public Validator getValidator()
    {
        return validatorFactory.getValidator();
    }

    public ValidatorContext usingContext()
    {
        return validatorFactory.usingContext();
    }

    public MessageInterpolator getMessageInterpolator()
    {
        return validatorFactory.getMessageInterpolator();
    }

    public TraversableResolver getTraversableResolver()
    {
        return validatorFactory.getTraversableResolver();
    }

    public ConstraintValidatorFactory getConstraintValidatorFactory()
    {
        return new BeanAwareConstraintValidatorFactory(validatorFactory.getConstraintValidatorFactory());
    }

    public <T> T unwrap(Class<T> tClass)
    {
        return validatorFactory.unwrap(tClass);
    }
}
