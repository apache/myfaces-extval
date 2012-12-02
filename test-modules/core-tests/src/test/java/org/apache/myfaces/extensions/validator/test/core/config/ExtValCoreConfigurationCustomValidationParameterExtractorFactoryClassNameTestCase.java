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

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DefaultValidationParameterExtractorFactory;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractor;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractorFactory;
import org.junit.Assert;
import org.junit.Test;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationCustomValidationParameterExtractorFactoryClassNameTestCase extends
        ExtValCoreConfigurationTestCase
{

    public static class CustomValidationParameterExtractorFactory implements ValidationParameterExtractorFactory
    {

        public ValidationParameterExtractor create()
        {
            return null;
        }

    }

    public static class Custom2ValidationParameterExtractorFactory implements ValidationParameterExtractorFactory
    {

        public ValidationParameterExtractor create()
        {
            return null;
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {

            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_VALIDATION_PARAMETER_EXTRACTOR_FACTORY",
                    CustomValidationParameterExtractorFactory.class.getName());
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
                public String customValidationParameterExtractorFactoryClassName()
                {
                    return Custom2ValidationParameterExtractorFactory.class.getName();
                }
            };
        }
        else
        {
            return null;
        }
    }

    @Test
    public void testCustomValidationParameterExtractorFactoryClassNameDefault()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(FactoryNames.VALIDATION_PARAMETER_EXTRACTOR_FACTORY, Object.class);
        Assert.assertEquals(DefaultValidationParameterExtractorFactory.class.getName(), factory.getClass().getName());

    }

    @Test
    public void testCustomValidationParameterExtractorFactoryClassNameWebXml()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(FactoryNames.VALIDATION_PARAMETER_EXTRACTOR_FACTORY, Object.class);
        Assert.assertEquals(CustomValidationParameterExtractorFactory.class.getName(), factory.getClass().getName());
    }

    @Test
    public void testCustomValidationParameterExtractorFactoryClassNameCustomConfig()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(FactoryNames.VALIDATION_PARAMETER_EXTRACTOR_FACTORY, Object.class);
        Assert.assertEquals(Custom2ValidationParameterExtractorFactory.class.getName(), factory.getClass().getName());
    }

}
