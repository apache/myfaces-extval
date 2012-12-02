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

import javax.faces.component.UIComponent;
import javax.faces.validator.ValidatorException;

import junit.framework.Test;

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.interceptor.ValidationExceptionInterceptor;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationCustomValidationExceptionInterceptorClassNameTestCase extends
        ExtValCoreConfigurationTestCase
{

    public ExtValCoreConfigurationCustomValidationExceptionInterceptorClassNameTestCase(String name)
    {
        super(name);
    }

    public static class CustomValidationExceptionInterceptor implements ValidationExceptionInterceptor
    {

        public boolean afterThrowing(UIComponent uiComponent, MetaDataEntry metaDataEntry, Object convertedObject,
                ValidatorException validatorException, ValidationStrategy validatorExceptionSource)
        {
            return false;
        }

    }

    public static class Custom2ValidationExceptionInterceptor implements ValidationExceptionInterceptor
    {

        public boolean afterThrowing(UIComponent uiComponent, MetaDataEntry metaDataEntry, Object convertedObject,
                ValidatorException validatorException, ValidationStrategy validatorExceptionSource)
        {
            return false;
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_VALIDATION_EXCEPTION_INTERCEPTOR",
                    CustomValidationExceptionInterceptor.class.getName());

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
                public String customValidationExceptionInterceptorClassName()
                {
                    return Custom2ValidationExceptionInterceptor.class.getName();
                }

            };
        }
        else
        {
            return null;
        }
    }

    public void testCustomValidationExceptionInterceptorClassNameDefault()
    {
        assertEquals(3, ExtValContext.getContext().getValidationExceptionInterceptors().size());
    }

    public void testCustomValidationExceptionInterceptorClassNameWebXml()
    {
        List<ValidationExceptionInterceptor> data = ExtValContext.getContext().getValidationExceptionInterceptors();
        assertEquals(4, data.size());
        assertEquals(CustomValidationExceptionInterceptor.class.getName(), data.get(3).getClass().getName());
    }

    public void testCustomValidationExceptionInterceptorClassNameCustomConfig()
    {
        List<ValidationExceptionInterceptor> data = ExtValContext.getContext().getValidationExceptionInterceptors();
        assertEquals(4, data.size());
        assertEquals(Custom2ValidationExceptionInterceptor.class.getName(), data.get(3).getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomValidationExceptionInterceptorClassNameTestCase.class);
    }

}
