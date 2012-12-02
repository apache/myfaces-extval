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

import java.lang.annotation.Annotation;
import java.util.List;
import java.util.Map;

import junit.framework.Test;

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DefaultValidationParameterExtractor;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DefaultValidationParameterExtractorFactory;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractor;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractorFactory;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationCustomValidationParameterExtractorClassNameTestCase extends
        ExtValCoreConfigurationTestCase
{

    public ExtValCoreConfigurationCustomValidationParameterExtractorClassNameTestCase(String name)
    {
        super(name);
    }

    public static class CustomValidationParameterExtractor implements ValidationParameterExtractor
    {

        public Map<Object, List<Object>> extract(Annotation annotation)
        {
            return null;
        }

        public List<Object> extract(Annotation annotation, Object key)
        {
            return null;
        }

        public <T> List<T> extract(Annotation annotation, Object key, Class<T> valueType)
        {
            return null;
        }

        public <T> T extract(Annotation annotation, Object key, Class<T> valueType, Class valueId)
        {
            return null;
        }

    }

    public static class Custom2ValidationParameterExtractor implements ValidationParameterExtractor
    {

        public Map<Object, List<Object>> extract(Annotation annotation)
        {
            return null;
        }

        public List<Object> extract(Annotation annotation, Object key)
        {
            return null;
        }

        public <T> List<T> extract(Annotation annotation, Object key, Class<T> valueType)
        {
            return null;
        }

        public <T> T extract(Annotation annotation, Object key, Class<T> valueType, Class valueId)
        {
            return null;
        }

    }

    @Override
    protected void addInitializationParameters()
    {

        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_VALIDATION_PARAMETER_EXTRACTOR",
                    CustomValidationParameterExtractor.class.getName());
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
                public String customValidationParameterExtractorClassName()
                {
                    return Custom2ValidationParameterExtractor.class.getName();
                }
            };
        }
        else
        {
            return null;
        }
    }

    public void testCustomValidationParameterExtractorClassNameDefault()
    {
        ValidationParameterExtractorFactory factory = new DefaultValidationParameterExtractorFactory();
        assertEquals(DefaultValidationParameterExtractor.class.getName(), factory.create().getClass().getName());

    }

    public void testCustomValidationParameterExtractorClassNameWebXml()
    {
        ValidationParameterExtractorFactory factory = new DefaultValidationParameterExtractorFactory();
        assertEquals(CustomValidationParameterExtractor.class.getName(), factory.create().getClass().getName());
    }

    public void testCustomValidationParameterExtractorClassNameCustomConfig()
    {
        ValidationParameterExtractorFactory factory = new DefaultValidationParameterExtractorFactory();
        assertEquals(Custom2ValidationParameterExtractor.class.getName(), factory.create().getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomValidationParameterExtractorClassNameTestCase.class);
    }

}
