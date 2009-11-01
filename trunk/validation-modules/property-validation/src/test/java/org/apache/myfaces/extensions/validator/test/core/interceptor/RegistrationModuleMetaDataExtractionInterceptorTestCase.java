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

import org.apache.myfaces.extensions.validator.test.AbstractExValViewControllerTestCase;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.InvocationOrder;
import org.apache.myfaces.extensions.validator.core.ValidationModuleAware;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.interceptor.MetaDataExtractionInterceptor;
import org.apache.myfaces.extensions.validator.PropertyValidationModuleKey;
import junit.framework.Test;
import junit.framework.TestSuite;

import java.util.List;

public class RegistrationModuleMetaDataExtractionInterceptorTestCase extends AbstractExValViewControllerTestCase
{
    /**
     * Construct a new instance of the test.
     *
     * @param name Name of the test.
     */
    public RegistrationModuleMetaDataExtractionInterceptorTestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(RegistrationModuleMetaDataExtractionInterceptorTestCase.class);
    }

    public void testModulePropertyValidationInterceptorInitialization()
    {
        resetExtValContext();

        ExtValContext.getContext().addMetaDataExtractionInterceptor(new TestGlobalPropertyValidationInterceptor());
        ExtValContext.getContext().addMetaDataExtractionInterceptor(new TestGlobalMetaDataExtractionInterceptor1000());
        ExtValContext.getContext().addMetaDataExtractionInterceptor(new TestModuleMetaDataExtractionInterceptor2());
        ExtValContext.getContext().addMetaDataExtractionInterceptor(new TestModuleMetaDataExtractionInterceptor3());
        ExtValContext.getContext().addMetaDataExtractionInterceptor(new TestGlobalMetaDataExtractionInterceptor1());

        checkGlobalOnlyPropertyValidationInterceptors();
        checkModuleAwarePropertyValidationInterceptorsWithTestModule();
        checkModuleAwarePropertyValidationInterceptorsWithPropertyValidationModule();
    }

    private void checkGlobalOnlyPropertyValidationInterceptors()
    {
        List<MetaDataExtractionInterceptor> result = ExtValContext.getContext().getMetaDataExtractionInterceptors();

        int resultLength = 3;
        assertEquals(resultLength, result.size());

        for(int i = 0; i < resultLength; i++)
        {
            switch (i)
            {
                case 0:
                    assertEquals(TestGlobalMetaDataExtractionInterceptor1.class, result.get(i).getClass());
                    break;
                case 1:
                    assertEquals(TestGlobalMetaDataExtractionInterceptor1000.class, result.get(i).getClass());
                    break;
                case 2:
                    assertEquals(TestGlobalPropertyValidationInterceptor.class, result.get(i).getClass());
                    break;
            }
        }
    }

    private void checkModuleAwarePropertyValidationInterceptorsWithTestModule()
    {
        List<MetaDataExtractionInterceptor> result = ExtValContext.getContext().getMetaDataExtractionInterceptorsFor(TestModule.class);

        int resultLength = 5;
        assertTrue(result.size() == resultLength);

        for(int i = 0; i < resultLength; i++)
        {
            switch (i)
            {
                case 0:
                    assertTrue(result.get(i) instanceof TestGlobalMetaDataExtractionInterceptor1);
                    break;
                case 1:
                    assertTrue(result.get(i) instanceof TestModuleMetaDataExtractionInterceptor2);
                    break;
                case 2:
                    assertTrue(result.get(i) instanceof TestModuleMetaDataExtractionInterceptor3);
                    break;
                case 3:
                    assertTrue(result.get(i) instanceof TestGlobalMetaDataExtractionInterceptor1000);
                    break;
                case 4:
                    assertTrue(result.get(i) instanceof TestGlobalPropertyValidationInterceptor);
                    break;
            }
        }
    }

    private void checkModuleAwarePropertyValidationInterceptorsWithPropertyValidationModule()
    {
        List<MetaDataExtractionInterceptor> result = ExtValContext.getContext().getMetaDataExtractionInterceptorsFor(PropertyValidationModuleKey.class);

        int resultLength = 4;
        assertTrue(result.size() == resultLength);

        for(int i = 0; i < resultLength; i++)
        {
            switch (i)
            {
                case 0:
                    assertEquals(TestGlobalMetaDataExtractionInterceptor1.class, result.get(i).getClass());
                    break;
                case 1:
                    assertEquals(TestModuleMetaDataExtractionInterceptor2.class, result.get(i).getClass());
                    break;
                case 2:
                    assertEquals(TestGlobalMetaDataExtractionInterceptor1000.class, result.get(i).getClass());
                    break;
                case 3:
                    assertEquals(TestGlobalPropertyValidationInterceptor.class, result.get(i).getClass());
                    break;
            }
        }
    }

    class TestGlobalPropertyValidationInterceptor implements MetaDataExtractionInterceptor
    {
        public void afterExtracting(PropertyInformation propertyInformation)
        {
        }
    }

    @InvocationOrder(1)
    class TestGlobalMetaDataExtractionInterceptor1 implements MetaDataExtractionInterceptor
    {
        public void afterExtracting(PropertyInformation propertyInformation)
        {
        }
    }

    @InvocationOrder(2)
    class TestModuleMetaDataExtractionInterceptor2 implements MetaDataExtractionInterceptor, ValidationModuleAware
    {
        public void afterExtracting(PropertyInformation propertyInformation)
        {
        }

        public String[] getModuleKeys()
        {
            return new String[] {PropertyValidationModuleKey.class.getName(), TestModule.class.getName()};
        }
    }

    @InvocationOrder(3)
    class TestModuleMetaDataExtractionInterceptor3 implements MetaDataExtractionInterceptor, ValidationModuleAware
    {
        public void afterExtracting(PropertyInformation propertyInformation)
        {
        }

        public String[] getModuleKeys()
        {
            return new String[] {TestModule.class.getName()};
        }
    }

    class TestModule
    {
    }

    @InvocationOrder(1000)
    class TestGlobalMetaDataExtractionInterceptor1000 implements MetaDataExtractionInterceptor
    {
        public void afterExtracting(PropertyInformation propertyInformation)
        {
        }
    }
}