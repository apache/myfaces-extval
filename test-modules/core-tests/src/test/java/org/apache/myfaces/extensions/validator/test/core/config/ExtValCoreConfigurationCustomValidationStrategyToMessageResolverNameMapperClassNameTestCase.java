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
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.factory.NameMapperAwareFactory;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.test.base.mock.MockMessageResolverFactory;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationCustomValidationStrategyToMessageResolverNameMapperClassNameTestCase extends
        ExtValCoreConfigurationTestCase
{

    private static final String WEB_XML = "Web.xml";
    private static final String CUSTOM_CONFIG = "Custom Config";

    public ExtValCoreConfigurationCustomValidationStrategyToMessageResolverNameMapperClassNameTestCase(String name)
    {
        super(name);
    }

    public static class CustomNameMapper implements NameMapper<ValidationStrategy>
    {

        public String createName(ValidationStrategy source)
        {
            return WEB_XML;
        }

    }

    public static class Custom2NameMapper implements NameMapper<ValidationStrategy>
    {

        public String createName(ValidationStrategy source)
        {
            return CUSTOM_CONFIG;
        }

    }

    @Override
    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".CUSTOM_VALIDATION_STRATEGY_TO_MESSAGE_RESOLVER_NAME_MAPPER", CustomNameMapper.class.getName());
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
                public String customValidationStrategyToMessageResolverNameMapperClassName()
                {
                    return Custom2NameMapper.class.getName();
                }
            };
        }
        else
        {
            return null;
        }
    }

    public void testCustomValidationStrategyToMessageResolverNameMapperClassNameDefault()
    {
        List<NameMapper<ValidationStrategy>> nameMappers = getNameMappers();
        assertEquals(5, nameMappers.size());
        // The first one (due to @InvocationOrder) is the
        // CustomConfiguredValidationStrategyToMsgResolverNameMapper which we
        // can
        // customize and testing here.
        NameMapper<ValidationStrategy> mapper = nameMappers.get(0);
        // By default nothing is configures so should return null.
        assertNull(mapper.createName(null));

    }

    public void testCustomValidationStrategyToMessageResolverNameMapperClassNameWebXml()
    {
        List<NameMapper<ValidationStrategy>> nameMappers = getNameMappers();
        assertEquals(5, nameMappers.size());
        // No mapper additional, but the first mapper contain now our custom
        // configured mapper.

        NameMapper<ValidationStrategy> mapper = nameMappers.get(0);
        // So now it should return some value
        assertEquals(WEB_XML, mapper.createName(null));
    }

    public void testCustomValidationStrategyToMessageResolverNameMapperClassNameCustomConfig()
    {
        List<NameMapper<ValidationStrategy>> nameMappers = getNameMappers();
        assertEquals(5, nameMappers.size());
        // No mapper additional, but the first mapper contain now our custom
        // configured mapper.

        NameMapper<ValidationStrategy> mapper = nameMappers.get(0);
        // So now it should return some value
        assertEquals(CUSTOM_CONFIG, mapper.createName(null));
    }

    private List<NameMapper<ValidationStrategy>> getNameMappers()
    {
        NameMapperAwareFactory result = ExtValContext.getContext().getFactoryFinder().getFactory(
                FactoryNames.MESSAGE_RESOLVER_FACTORY, NameMapperAwareFactory.class);

        return ((MockMessageResolverFactory) result).getRegisteredNameMapperList();
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomValidationStrategyToMessageResolverNameMapperClassNameTestCase.class);
    }

}
