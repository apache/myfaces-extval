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

import java.util.Map;

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.ComponentMetaDataExtractorFactory;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.DefaultComponentMetaDataExtractorFactory;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.junit.Assert;
import org.junit.Test;


/**
 * 
 * @author Rudy De Busscher
 * since v4
 *
 */
public class ExtValCoreConfigurationCustomComponentMetaDataExtractorFactoryClassNameTestCase extends
        ExtValCoreConfigurationTestCase
{

    public static class CustomComponentMetaDataExtractorFactory implements ComponentMetaDataExtractorFactory
    {

        public MetaDataExtractor create()
        {

            return null;
        }

        public MetaDataExtractor createWith(Map<String, Object> properties)
        {

            return null;
        }

    }

    public static class Custom2ComponentMetaDataExtractorFactory implements ComponentMetaDataExtractorFactory
    {

        public MetaDataExtractor create()
        {

            return null;
        }

        public MetaDataExtractor createWith(Map<String, Object> properties)
        {

            return null;
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_COMPONENT_META_DATA_EXTRACTOR_FACTORY",
                    CustomComponentMetaDataExtractorFactory.class.getName());

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
                public String customComponentMetaDataExtractorFactoryClassName()
                {

                    return Custom2ComponentMetaDataExtractorFactory.class.getName();
                }

            };
        }
        else
        {
            return null;
        }
    }

    @Test
    public void testCustomComponentMetaDataExtractorFactoryClassNameDefault()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(FactoryNames.COMPONENT_META_DATA_EXTRACTOR_FACTORY,
                ComponentMetaDataExtractorFactory.class);
        Assert.assertEquals(DefaultComponentMetaDataExtractorFactory.class.getName(), factory.getClass().getName());
    }

    @Test
    public void testCustomComponentMetaDataExtractorFactoryClassNameWebXml()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(FactoryNames.COMPONENT_META_DATA_EXTRACTOR_FACTORY,
                ComponentMetaDataExtractorFactory.class);
        Assert.assertEquals(CustomComponentMetaDataExtractorFactory.class.getName(), factory.getClass().getName());
    }

    @Test
    public void testCustomComponentMetaDataExtractorFactoryClassNameCustomConfig()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(FactoryNames.COMPONENT_META_DATA_EXTRACTOR_FACTORY,
                ComponentMetaDataExtractorFactory.class);
        Assert.assertEquals(Custom2ComponentMetaDataExtractorFactory.class.getName(), factory.getClass().getName());
    }

}
