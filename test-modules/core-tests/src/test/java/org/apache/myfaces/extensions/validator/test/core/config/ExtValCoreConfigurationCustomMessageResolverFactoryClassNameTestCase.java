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
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.DefaultMessageResolverFactory;
import org.apache.myfaces.extensions.validator.test.base.mock.MockMessageResolverFactory;
import org.junit.Assert;
import org.junit.Test;


/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationCustomMessageResolverFactoryClassNameTestCase extends
        ExtValCoreConfigurationTestCase
{


    public static class CustomMessageResolverFactory extends DefaultMessageResolverFactory
    {

    }

    public static class Custom2MessageResolverFactory extends DefaultMessageResolverFactory
    {

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_MESSAGE_RESOLVER_FACTORY",
                    CustomMessageResolverFactory.class.getName());

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
                public String customMessageResolverFactoryClassName()
                {
                    return Custom2MessageResolverFactory.class.getName();
                }
            };
        }
        else
        {
            return null;
        }
    }

    @Test
    public void testCustomMessageResolverFactoryClassNameDefault()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(FactoryNames.MESSAGE_RESOLVER_FACTORY, Object.class);
        // The TestCase setup registers a Mockfactory so that a protected method
        // is made visible.
        // assertEquals(DefaultMessageResolverFactory.class.getName(), factory
        // .getClass().getName());
        Assert.assertEquals(MockMessageResolverFactory.class.getName(), factory.getClass().getName());

    }

    @Test
    public void testCustomMessageResolverFactoryClassNameWebXml()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(FactoryNames.MESSAGE_RESOLVER_FACTORY, Object.class);
        Assert.assertEquals(CustomMessageResolverFactory.class.getName(), factory.getClass().getName());
    }

    @Test
    public void testCustomMessageResolverFactoryClassNameCustomConfig()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(FactoryNames.MESSAGE_RESOLVER_FACTORY, Object.class);
        Assert.assertEquals(Custom2MessageResolverFactory.class.getName(), factory.getClass().getName());
    }

}
