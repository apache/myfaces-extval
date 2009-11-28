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
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.validation.ConstraintValidatorFactory;
import javax.validation.ConstraintValidator;
import java.beans.Introspector;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
class BeanAwareConstraintValidatorFactory implements ConstraintValidatorFactory
{
    private ConstraintValidatorFactory constraintValidatorFactory;

    public BeanAwareConstraintValidatorFactory(ConstraintValidatorFactory constraintValidatorFactory)
    {
        setConstraintValidatorFactory(constraintValidatorFactory);
    }

    public void setConstraintValidatorFactory(ConstraintValidatorFactory constraintValidatorFactory)
    {
        if(constraintValidatorFactory == null)
        {
            throw new IllegalStateException("null is not allowed here");
        }
        this.constraintValidatorFactory = constraintValidatorFactory;
    }

    @SuppressWarnings({"unchecked"})
    @ToDo(value = Priority.MEDIUM, description = "allow the registration of a custom prefix")
    public <T extends ConstraintValidator<?, ?>> T getInstance(Class<T> targetClass)
    {
        String validatorClassName = targetClass.getSimpleName();
        Object result = ExtValUtils.getELHelper().getBean(createBeanName(validatorClassName));

        if(result != null && targetClass.isAssignableFrom(result.getClass()))
        {
            return (T)result;
        }
        return this.constraintValidatorFactory.getInstance(targetClass);
    }

    private String createBeanName(String validatorClassName)
    {
        return Introspector.decapitalize(validatorClassName);
    }
}