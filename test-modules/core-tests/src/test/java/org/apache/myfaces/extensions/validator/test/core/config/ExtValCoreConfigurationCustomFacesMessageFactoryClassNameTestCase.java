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

import javax.faces.application.FacesMessage;
import javax.faces.application.FacesMessage.Severity;

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FacesMessageFactory;
import org.apache.myfaces.extensions.validator.core.factory.FactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.validation.message.DefaultFacesMessageFactory;
import org.junit.Assert;
import org.junit.Test;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationCustomFacesMessageFactoryClassNameTestCase extends ExtValCoreConfigurationTestCase
{

    public static class CustomFacesMessageFactory implements FacesMessageFactory
    {

        public FacesMessage convert(FacesMessage facesMessage)
        {
            return null;
        }

        public FacesMessage create(Severity severity, String summary, String detail)
        {
            return null;
        }

    }

    public static class Custom2FacesMessageFactory implements FacesMessageFactory
    {

        public FacesMessage convert(FacesMessage facesMessage)
        {
            return null;
        }

        public FacesMessage create(Severity severity, String summary, String detail)
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
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_FACES_MESSAGE_FACTORY",
                    CustomFacesMessageFactory.class.getName());

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
                public String customFacesMessageFactoryClassName()
                {
                    return Custom2FacesMessageFactory.class.getName();
                }
            };
        }
        else
        {
            return null;
        }
    }

    @Test
    public void testCustomFacesMessageFactoryClassNameDefault()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(FactoryNames.FACES_MESSAGE_FACTORY, FacesMessageFactory.class);
        Assert.assertEquals(DefaultFacesMessageFactory.class.getName(), factory.getClass().getName());

    }

    @Test
    public void testCustomFacesMessageFactoryClassNameWebXml()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(FactoryNames.FACES_MESSAGE_FACTORY, FacesMessageFactory.class);
        Assert.assertEquals(CustomFacesMessageFactory.class.getName(), factory.getClass().getName());
    }

    @Test
    public void testCustomFacesMessageFactoryClassNameCustomConfig()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(FactoryNames.FACES_MESSAGE_FACTORY, FacesMessageFactory.class);
        Assert.assertEquals(Custom2FacesMessageFactory.class.getName(), factory.getClass().getName());
    }

}
