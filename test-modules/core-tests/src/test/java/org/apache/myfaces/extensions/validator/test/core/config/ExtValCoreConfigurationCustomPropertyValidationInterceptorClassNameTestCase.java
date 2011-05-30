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
package org.apache.myfaces.extensions.validator.test.core.config;

import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.interceptor.PropertyValidationInterceptor;
import org.junit.Assert;
import org.junit.Test;

/**
 * 
 * @author Rudy De Busscher
 * since v4
 *
 */
public class ExtValCoreConfigurationCustomPropertyValidationInterceptorClassNameTestCase extends
        ExtValCoreConfigurationTestCase
{


    public static class CustomPropertyValidationInterceptor implements PropertyValidationInterceptor
    {

        public void afterValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject,
                Map<String, Object> properties)
        {

        }

        public boolean beforeValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject,
                Map<String, Object> properties)
        {
            return false;
        }

    }

    public static class Custom2PropertyValidationInterceptor implements PropertyValidationInterceptor
    {

        public void afterValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject,
                Map<String, Object> properties)
        {

        }

        public boolean beforeValidation(FacesContext facesContext, UIComponent uiComponent, Object convertedObject,
                Map<String, Object> properties)
        {
            return false;
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_PROPERTY_VALIDATION_INTERCEPTOR",
                    CustomPropertyValidationInterceptor.class.getName());
        }
    }

    @Override
    protected ExtValCoreConfiguration getCustomExtValCoreConfiguration()
    {
        if (needCustomConfig())
        {

            return new DefaultExtValCoreConfiguration()
            {

                @Override
                public String customPropertyValidationInterceptorClassName()
                {
                    return Custom2PropertyValidationInterceptor.class.getName();
                }

            };
        }
        else
        {
            return null;
        }
    }

    @Test
    public void testCustomPropertyValidationInterceptorClassNameDefault()
    {
        Assert.assertEquals(1, ExtValContext.getContext().getPropertyValidationInterceptors().size());
    }

    @Test
    public void testCustomPropertyValidationInterceptorClassNameWebXml()
    {
        List<PropertyValidationInterceptor> data = ExtValContext.getContext().getPropertyValidationInterceptors();
        Assert.assertEquals(2, data.size());
        Assert.assertEquals(CustomPropertyValidationInterceptor.class.getName(), data.get(1).getClass().getName());
    }

    @Test
    public void testCustomPropertyValidationInterceptorClassNameCustomConfig()
    {
        List<PropertyValidationInterceptor> data = ExtValContext.getContext().getPropertyValidationInterceptors();
        Assert.assertEquals(2, data.size());
        Assert.assertEquals(Custom2PropertyValidationInterceptor.class.getName(), data.get(1).getClass().getName());
    }


}
