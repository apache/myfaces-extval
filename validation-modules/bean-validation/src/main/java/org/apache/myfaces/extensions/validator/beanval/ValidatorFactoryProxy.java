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

import org.apache.myfaces.extensions.validator.beanval.util.BeanValidationUtils;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.validation.ConstraintValidatorFactory;
import javax.validation.MessageInterpolator;
import javax.validation.TraversableResolver;
import javax.validation.Validator;
import javax.validation.ValidatorContext;
import javax.validation.ValidatorFactory;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @since r5
 */
//see EXTVAL-130
public class ValidatorFactoryProxy implements ValidatorFactory
{
    private ValidatorFactory validatorFactory;

    protected ValidatorFactory getValidatorFactory()
    {
        if(this.validatorFactory != null)
        {
            return validatorFactory;
        }

        ValidatorFactory validatorFactory;

        Object contextAwareValidatorFactory = null;

        if (isMyFacesCodiBeanValidationModuleAvailable())
        {
            try
            {
                contextAwareValidatorFactory = ExtValUtils.getELHelper().getBean("contextAwareValidatorFactory");
            }
            catch (Exception e)
            {
                Logger logger = Logger.getLogger(getClass().getName());

                if (logger.isLoggable(Level.WARNING))
                {
                    logger.log(Level.WARNING, "failed to lookup a bean with the name contextAwareValidatorFactory", e);
                }
            }
        }

        if (contextAwareValidatorFactory instanceof ValidatorFactory)
        {
            validatorFactory = (ValidatorFactory) contextAwareValidatorFactory;
        }
        else
        {
            validatorFactory = new BeanAwareValidatorFactory(BeanValidationUtils.getDefaultValidatorFactory());
        }

        this.validatorFactory = validatorFactory;
        return validatorFactory;
    }

    public Validator getValidator()
    {
        return getValidatorFactory().getValidator();
    }

    protected boolean isMyFacesCodiBeanValidationModuleAvailable()
    {
        Class result = ClassUtils
                .tryToLoadClassForName("org.apache.myfaces.extensions.cdi.bv.api.BeanValidationModuleBeanNames");

        return result != null;
    }

    public ValidatorContext usingContext()
    {
        return getValidatorFactory().usingContext();
    }

    public MessageInterpolator getMessageInterpolator()
    {
        return getValidatorFactory().getMessageInterpolator();
    }

    public TraversableResolver getTraversableResolver()
    {
        return getValidatorFactory().getTraversableResolver();
    }

    public ConstraintValidatorFactory getConstraintValidatorFactory()
    {
        return getValidatorFactory().getConstraintValidatorFactory();
    }

    public <T> T unwrap(Class<T> type)
    {
        return getValidatorFactory().unwrap(type);
    }
}
