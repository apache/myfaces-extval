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
import org.apache.myfaces.extensions.validator.core.interceptor.PropertyValidationInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.FacesMessagePropertyValidationInterceptor;
import org.apache.myfaces.extensions.validator.test.core.AbstractExValCoreTestCase;
import junit.framework.Test;
import junit.framework.TestSuite;
import junit.framework.Assert;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import java.util.List;
import java.util.Map;

public class RegistrationPropertyValidationInterceptorTestCase extends AbstractExValCoreTestCase
{
    /**
     * Construct a new instance of the test.
     *
     * @param name Name of the test.
     */
    public RegistrationPropertyValidationInterceptorTestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(RegistrationPropertyValidationInterceptorTestCase.class);
    }

    public void testPropertyValidationInterceptorInitialization()
    {
        ExtValContext.getContext().addPropertyValidationInterceptor(new TestComponentInitializer());
        ExtValContext.getContext().addPropertyValidationInterceptor(new TestMetaDataExtractionInterceptor1000());
        ExtValContext.getContext().addPropertyValidationInterceptor(new TestMetaDataExtractionInterceptor2());
        ExtValContext.getContext().addPropertyValidationInterceptor(new TestMetaDataExtractionInterceptor3());
        ExtValContext.getContext().addPropertyValidationInterceptor(new TestMetaDataExtractionInterceptor1());

        List<PropertyValidationInterceptor> result = ExtValContext.getContext().getPropertyValidationInterceptors();

        int resultLength = 6;
        Assert.assertEquals(resultLength, result.size());

        for(int i = 0; i < resultLength; i++)
        {
            switch (i)
            {
                case 0:
                    Assert.assertEquals(TestMetaDataExtractionInterceptor1.class, result.get(i).getClass());
                    break;
                case 1:
                    Assert.assertEquals(TestMetaDataExtractionInterceptor2.class, result.get(i).getClass());
                    break;
                case 2:
                    Assert.assertEquals(TestMetaDataExtractionInterceptor3.class, result.get(i).getClass());
                    break;
                case 3:
                    Assert.assertEquals(FacesMessagePropertyValidationInterceptor.class, result.get(i).getClass());
                    break;
                case 4:
                    Assert.assertEquals(TestMetaDataExtractionInterceptor1000.class, result.get(i).getClass());
                    break;
                case 5:
                    Assert.assertEquals(TestComponentInitializer.class, result.get(i).getClass());
                    break;
            }
        }
    }

    class TestComponentInitializer implements PropertyValidationInterceptor
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
    class TestMetaDataExtractionInterceptor1 implements PropertyValidationInterceptor
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
    class TestMetaDataExtractionInterceptor2 implements PropertyValidationInterceptor
    {
        public boolean beforeValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject, Map<String, Object> properties)
        {
            return true;
        }

        public void afterValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject, Map<String, Object> properties)
        {
        }
    }

    @InvocationOrder(3)
    class TestMetaDataExtractionInterceptor3 implements PropertyValidationInterceptor
    {
        public boolean beforeValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject, Map<String, Object> properties)
        {
            return true;
        }

        public void afterValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject, Map<String, Object> properties)
        {
        }
    }

    @InvocationOrder(1000)
    class TestMetaDataExtractionInterceptor1000 implements PropertyValidationInterceptor
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