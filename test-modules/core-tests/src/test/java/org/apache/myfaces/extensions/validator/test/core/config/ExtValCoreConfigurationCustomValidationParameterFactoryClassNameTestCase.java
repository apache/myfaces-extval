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

import junit.framework.Test;

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DefaultValidationParameterFactory;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationCustomValidationParameterFactoryClassNameTestCase extends
        ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationCustomValidationParameterFactoryClassNameTestCase(String name)
    {
        super(name);
    }

    public static class CustomValidationParameterFactory extends DefaultValidationParameterFactory
    {

    }

    public static class Custom2ValidationParameterFactory extends DefaultValidationParameterFactory
    {

    }

    @Override
    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {

            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_VALIDATION_PARAMETER_FACTORY",
                    CustomValidationParameterFactory.class.getName());

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
                public String customValidationParameterFactoryClassName()
                {
                    return Custom2ValidationParameterFactory.class.getName();
                }
            };
        }
        else
        {
            return null;
        }
    }

    public void testCustomValidationParameterFactoryClassNameDefault()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(FactoryNames.VALIDATION_PARAMETER_FACTORY, Object.class);
        assertEquals(DefaultValidationParameterFactory.class.getName(), factory.getClass().getName());

    }

    public void testCustomValidationParameterFactoryClassNameWebXml()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(FactoryNames.VALIDATION_PARAMETER_FACTORY, Object.class);
        assertEquals(CustomValidationParameterFactory.class.getName(), factory.getClass().getName());
    }

    public void testCustomValidationParameterFactoryClassNameCustomConfig()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(FactoryNames.VALIDATION_PARAMETER_FACTORY, Object.class);
        assertEquals(Custom2ValidationParameterFactory.class.getName(), factory.getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(ExtValCoreConfigurationCustomValidationParameterFactoryClassNameTestCase.class);
    }

}
