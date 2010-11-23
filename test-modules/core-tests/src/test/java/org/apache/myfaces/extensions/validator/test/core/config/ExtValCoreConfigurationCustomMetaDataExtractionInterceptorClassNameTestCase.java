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

import junit.framework.Test;

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.interceptor.MetaDataExtractionInterceptor;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

/**
 * 
 * @author Rudy De Busscher
 * since v4
 *
 */
public class ExtValCoreConfigurationCustomMetaDataExtractionInterceptorClassNameTestCase extends
        ExtValCoreConfigurationTestCase
{

    public ExtValCoreConfigurationCustomMetaDataExtractionInterceptorClassNameTestCase(String name)
    {
        super(name);
    }

    public static class CustomMetaDataExtractionInterceptor implements MetaDataExtractionInterceptor
    {

        public void afterExtracting(PropertyInformation propertyInformation)
        {
        }

    }

    public static class Custom2MetaDataExtractionInterceptor implements MetaDataExtractionInterceptor
    {

        public void afterExtracting(PropertyInformation propertyInformation)
        {
        }

    }

    @Override
    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_META_DATA_EXTRACTION_INTERCEPTOR",
                    CustomMetaDataExtractionInterceptor.class.getName());
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
                public String customMetaDataExtractionInterceptorClassName()
                {
                    return Custom2MetaDataExtractionInterceptor.class.getName();
                }
            };
        }
        else
        {
            return null;
        }
    }

    public void testCustomMetaDataExtractionInterceptorClassNameDefault()
    {
        assertEquals(0, ExtValContext.getContext().getMetaDataExtractionInterceptors().size());
    }

    public void testCustomMetaDataExtractionInterceptorClassNameWebXml()
    {
        List<MetaDataExtractionInterceptor> result = ExtValContext.getContext().getMetaDataExtractionInterceptors();
        assertEquals(1, result.size());
        assertEquals(CustomMetaDataExtractionInterceptor.class.getName(), result.get(0).getClass().getName());
    }

    public void testCustomMetaDataExtractionInterceptorClassNameCustomConfig()
    {
        List<MetaDataExtractionInterceptor> result = ExtValContext.getContext().getMetaDataExtractionInterceptors();
        assertEquals(1, result.size());
        assertEquals(Custom2MetaDataExtractionInterceptor.class.getName(), result.get(0).getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomMetaDataExtractionInterceptorClassNameTestCase.class);
    }

}
