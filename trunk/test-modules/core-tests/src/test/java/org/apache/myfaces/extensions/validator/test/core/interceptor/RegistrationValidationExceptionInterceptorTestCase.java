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
package org.apache.myfaces.extensions.validator.test.core.interceptor;

import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.InvocationOrder;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.interceptor.ValidationExceptionInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.HtmlCoreComponentsValidationExceptionInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.ViolationSeverityValidationExceptionInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.ViolationExceptionInterceptor;
import org.apache.myfaces.extensions.validator.test.core.AbstractExValCoreTestCase;
import junit.framework.Test;
import junit.framework.TestSuite;
import junit.framework.Assert;

import javax.faces.component.UIComponent;
import javax.faces.validator.ValidatorException;
import java.util.List;

public class RegistrationValidationExceptionInterceptorTestCase extends AbstractExValCoreTestCase
{
    /**
     * Construct a new instance of the test.
     *
     * @param name Name of the test.
     */
    public RegistrationValidationExceptionInterceptorTestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(RegistrationValidationExceptionInterceptorTestCase.class);
    }

    public void testValidationExceptionInterceptorInitialization()
    {
        ExtValContext.getContext().addValidationExceptionInterceptor(new TestValidationExceptionInterceptor1());
        ExtValContext.getContext().addValidationExceptionInterceptor(new TestValidationExceptionInterceptor2());
        ExtValContext.getContext().addValidationExceptionInterceptor(new TestValidationExceptionInterceptor3());
        ExtValContext.getContext().addValidationExceptionInterceptor(new TestValidationExceptionInterceptor1000());
        ExtValContext.getContext().addValidationExceptionInterceptor(new TestValidationExceptionInterceptor());

        List<ValidationExceptionInterceptor> result = ExtValContext.getContext().getValidationExceptionInterceptors();

        int resultLength = 8;
        Assert.assertEquals(resultLength, result.size());

        for(int i = 0; i < resultLength; i++)
        {
            switch (i)
            {
                case 0:
                    Assert.assertEquals(TestValidationExceptionInterceptor1.class, result.get(i).getClass());
                    break;
                case 1:
                    Assert.assertEquals(TestValidationExceptionInterceptor2.class, result.get(i).getClass());
                    break;
                case 2:
                    Assert.assertEquals(TestValidationExceptionInterceptor3.class, result.get(i).getClass());
                    break;
                case 3:
                    Assert.assertEquals(ViolationSeverityValidationExceptionInterceptor.class, result.get(i).getClass());
                    break;
                case 4:
                    Assert.assertEquals(HtmlCoreComponentsValidationExceptionInterceptor.class, result.get(i).getClass());
                    break;
                case 5:
                    Assert.assertEquals(ViolationExceptionInterceptor.class, result.get(i).getClass());
                    break;
                case 6:
                    Assert.assertEquals(TestValidationExceptionInterceptor1000.class, result.get(i).getClass());
                    break;
                case 7:
                    Assert.assertEquals(TestValidationExceptionInterceptor.class, result.get(i).getClass());
                    break;
            }
        }
    }

    class TestValidationExceptionInterceptor implements ValidationExceptionInterceptor
    {
        public boolean afterThrowing(UIComponent uiComponent, MetaDataEntry metaDataEntry, Object convertedObject, ValidatorException validatorException, ValidationStrategy validatorExceptionSource)
        {
            return true;
        }
    }

    @InvocationOrder(1)
    class TestValidationExceptionInterceptor1 implements ValidationExceptionInterceptor
    {
        public boolean afterThrowing(UIComponent uiComponent, MetaDataEntry metaDataEntry, Object convertedObject, ValidatorException validatorException, ValidationStrategy validatorExceptionSource)
        {
            return true;
        }
    }

    @InvocationOrder(2)
    class TestValidationExceptionInterceptor2 implements ValidationExceptionInterceptor
    {
        public boolean afterThrowing(UIComponent uiComponent, MetaDataEntry metaDataEntry, Object convertedObject, ValidatorException validatorException, ValidationStrategy validatorExceptionSource)
        {
            return true;
        }
    }

    @InvocationOrder(3)
    class TestValidationExceptionInterceptor3 implements ValidationExceptionInterceptor
    {
        public boolean afterThrowing(UIComponent uiComponent, MetaDataEntry metaDataEntry, Object convertedObject, ValidatorException validatorException, ValidationStrategy validatorExceptionSource)
        {
            return true;
        }
    }

    @InvocationOrder(1000)
    class TestValidationExceptionInterceptor1000 implements ValidationExceptionInterceptor
    {
        public boolean afterThrowing(UIComponent uiComponent, MetaDataEntry metaDataEntry, Object convertedObject, ValidatorException validatorException, ValidationStrategy validatorExceptionSource)
        {
            return true;
        }
    }
}
