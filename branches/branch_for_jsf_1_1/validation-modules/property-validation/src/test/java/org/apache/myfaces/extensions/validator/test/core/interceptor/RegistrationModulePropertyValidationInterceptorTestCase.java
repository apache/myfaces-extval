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
import org.apache.myfaces.extensions.validator.core.interceptor.PropertyValidationInterceptor;
import org.apache.myfaces.extensions.validator.PropertyValidationModuleKey;
import junit.framework.Test;
import junit.framework.TestSuite;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import java.util.List;
import java.util.Map;

public class RegistrationModulePropertyValidationInterceptorTestCase extends AbstractExValViewControllerTestCase
{
    /**
     * Construct a new instance of the test.
     *
     * @param name Name of the test.
     */
    public RegistrationModulePropertyValidationInterceptorTestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(RegistrationModulePropertyValidationInterceptorTestCase.class);
    }

    public void testModulePropertyValidationInterceptorInitialization()
    {
        resetExtValContext();

        ExtValContext.getContext().addPropertyValidationInterceptor(new TestGlobalPropertyValidationInterceptor());
        ExtValContext.getContext().addPropertyValidationInterceptor(new TestGlobalPropertyValidationInterceptor1000());
        ExtValContext.getContext().addPropertyValidationInterceptor(new TestModulePropertyValidationInterceptor2());
        ExtValContext.getContext().addPropertyValidationInterceptor(new TestModulePropertyValidationInterceptor3());
        ExtValContext.getContext().addPropertyValidationInterceptor(new TestGlobalPropertyValidationInterceptor1());

        checkGlobalOnlyPropertyValidationInterceptors();
        checkModuleAwarePropertyValidationInterceptorsWithTestModule();
        checkModuleAwarePropertyValidationInterceptorsWithPropertyValidationModule();
    }

    private void checkGlobalOnlyPropertyValidationInterceptors()
    {
        List<PropertyValidationInterceptor> result = ExtValContext.getContext().getPropertyValidationInterceptors();

        int resultLength = 3;
        assertEquals(resultLength, result.size());

        for(int i = 0; i < resultLength; i++)
        {
            switch (i)
            {
                case 0:
                    assertEquals(TestGlobalPropertyValidationInterceptor1.class, result.get(i).getClass());
                    break;
                case 1:
                    assertEquals(TestGlobalPropertyValidationInterceptor1000.class, result.get(i).getClass());
                    break;
                case 2:
                    assertEquals(TestGlobalPropertyValidationInterceptor.class, result.get(i).getClass());
                    break;
            }
        }
    }

    private void checkModuleAwarePropertyValidationInterceptorsWithTestModule()
    {
        List<PropertyValidationInterceptor> result = ExtValContext.getContext().getPropertyValidationInterceptorsFor(TestModule.class);

        int resultLength = 5;
        assertTrue(result.size() == resultLength);

        for(int i = 0; i < resultLength; i++)
        {
            switch (i)
            {
                case 0:
                    assertTrue(result.get(i) instanceof TestGlobalPropertyValidationInterceptor1);
                    break;
                case 1:
                    assertTrue(result.get(i) instanceof TestModulePropertyValidationInterceptor2);
                    break;
                case 2:
                    assertTrue(result.get(i) instanceof TestModulePropertyValidationInterceptor3);
                    break;
                case 3:
                    assertTrue(result.get(i) instanceof TestGlobalPropertyValidationInterceptor1000);
                    break;
                case 4:
                    assertTrue(result.get(i) instanceof TestGlobalPropertyValidationInterceptor);
                    break;
            }
        }
    }

    private void checkModuleAwarePropertyValidationInterceptorsWithPropertyValidationModule()
    {
        List<PropertyValidationInterceptor> result = ExtValContext.getContext().getPropertyValidationInterceptorsFor(PropertyValidationModuleKey.class);

        int resultLength = 4;
        assertTrue(result.size() == resultLength);

        for(int i = 0; i < resultLength; i++)
        {
            switch (i)
            {
                case 0:
                    assertEquals(TestGlobalPropertyValidationInterceptor1.class, result.get(i).getClass());
                    break;
                case 1:
                    assertEquals(TestModulePropertyValidationInterceptor2.class, result.get(i).getClass());
                    break;
                case 2:
                    assertEquals(TestGlobalPropertyValidationInterceptor1000.class, result.get(i).getClass());
                    break;
                case 3:
                    assertEquals(TestGlobalPropertyValidationInterceptor.class, result.get(i).getClass());
                    break;
            }
        }
    }

    class TestGlobalPropertyValidationInterceptor implements PropertyValidationInterceptor
    {
        public boolean beforeValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject, Map<String, Object> properties)
        {
            return true;
        }

        public void afterValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject, Map<String, Object> properties)
        {
        }
    }

    @InvocationOrder(1)
    class TestGlobalPropertyValidationInterceptor1 implements PropertyValidationInterceptor
    {
        public boolean beforeValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject, Map<String, Object> properties)
        {
            return true;
        }

        public void afterValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject, Map<String, Object> properties)
        {
        }
    }

    @InvocationOrder(2)
    class TestModulePropertyValidationInterceptor2 implements PropertyValidationInterceptor, ValidationModuleAware
    {
        public boolean beforeValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject, Map<String, Object> properties)
        {
            return true;
        }

        public void afterValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject, Map<String, Object> properties)
        {
        }

        public String[] getModuleKeys()
        {
            return new String[] {PropertyValidationModuleKey.class.getName(), TestModule.class.getName()};
        }
    }

    @InvocationOrder(3)
    class TestModulePropertyValidationInterceptor3 implements PropertyValidationInterceptor, ValidationModuleAware
    {
        public boolean beforeValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject, Map<String, Object> properties)
        {
            return true;
        }

        public void afterValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject, Map<String, Object> properties)
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
    class TestGlobalPropertyValidationInterceptor1000 implements PropertyValidationInterceptor
    {
        public boolean beforeValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject, Map<String, Object> properties)
        {
            return true;
        }

        public void afterValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject, Map<String, Object> properties)
        {
        }
    }
}
