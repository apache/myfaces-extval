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
public class ExtValCoreConfigurationDeactivateDefaultNameMappersTestCase extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationDeactivateDefaultNameMappersTestCase(String name)
    {
        super(name);
    }

    @Override
    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".DEACTIVATE_DEFAULT_NAME_MAPPERS", "true");
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
                public boolean deactivateDefaultNameMappers()
                {
                    return true;
                }

            };
        }
        else
        {
            return null;
        }
    }

    public void testDeactivateDefaultNameMappersDefault()
    {
        List<NameMapper<ValidationStrategy>> nameMappers = getNameMappers();
        assertFalse(nameMappers.isEmpty());
    }

    public void testDeactivateDefaultNameMappersWebXml()
    {
        List<NameMapper<ValidationStrategy>> nameMappers = getNameMappers();
        assertTrue(nameMappers.isEmpty());
    }

    // Name shouldn't contain default
    public void testDeactivateDefNameMappersCustomConfig()
    {
        List<NameMapper<ValidationStrategy>> nameMappers = getNameMappers();
        assertTrue(nameMappers.isEmpty());
    }

    private List<NameMapper<ValidationStrategy>> getNameMappers()
    {
        NameMapperAwareFactory result = ExtValContext.getContext().getFactoryFinder().getFactory(
                FactoryNames.MESSAGE_RESOLVER_FACTORY, NameMapperAwareFactory.class);

        return ((MockMessageResolverFactory) result).getRegisteredNameMapperList();
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(ExtValCoreConfigurationDeactivateDefaultNameMappersTestCase.class);
    }

}
