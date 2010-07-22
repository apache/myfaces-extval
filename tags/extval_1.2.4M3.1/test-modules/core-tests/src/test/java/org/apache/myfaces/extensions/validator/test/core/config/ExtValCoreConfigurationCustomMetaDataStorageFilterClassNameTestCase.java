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

import java.lang.reflect.Field;
import java.util.List;

import junit.framework.Test;

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.storage.DefaultMetaDataStorage;
import org.apache.myfaces.extensions.validator.core.storage.MetaDataStorageFilter;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

/**
 * 
 * @author Rudy De Busscher
 * since v4
 *
 */
public class ExtValCoreConfigurationCustomMetaDataStorageFilterClassNameTestCase extends
        ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationCustomMetaDataStorageFilterClassNameTestCase(String name)
    {
        super(name);
    }

    public static class CustomMetaDataStorageFilter implements MetaDataStorageFilter
    {

        public void filter(PropertyInformation propertyInformation)
        {

        }

    }

    public static class Custom2MetaDataStorageFilter implements MetaDataStorageFilter
    {

        public void filter(PropertyInformation propertyInformation)
        {

        }

    }

    @Override
    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_META_DATA_STORAGE_FILTER",
                    CustomMetaDataStorageFilter.class.getName());
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
                public String customMetaDataStorageFilterClassName()
                {
                    return Custom2MetaDataStorageFilter.class.getName();
                }
            };
        }
        else
        {
            return null;
        }
    }

    public void testCustomMetaDataStorageFilterClassNameDefault() throws Exception
    {
        DefaultMetaDataStorage metaDataStorage = new DefaultMetaDataStorage();
        // A little reflection stuff because filters can not be exposed in any
        // way.
        Field field = DefaultMetaDataStorage.class.getDeclaredField("metaDataStorageFilters");
        field.setAccessible(true);
        Object data = field.get(metaDataStorage);
        assertNotNull(data);
        List<MetaDataStorageFilter> metaDataStorageFilters = (List<MetaDataStorageFilter>) data;
        assertEquals(0, metaDataStorageFilters.size());
    }

    public void testCustomMetaDataStorageFilterClassNameWebXml() throws Exception
    {
        DefaultMetaDataStorage metaDataStorage = new DefaultMetaDataStorage();
        // A little reflection stuff because filters can not be exposed in any
        // way.
        Field field = DefaultMetaDataStorage.class.getDeclaredField("metaDataStorageFilters");
        field.setAccessible(true);
        Object data = field.get(metaDataStorage);
        assertNotNull(data);
        List<MetaDataStorageFilter> metaDataStorageFilters = (List<MetaDataStorageFilter>) data;
        assertEquals(1, metaDataStorageFilters.size());
        assertEquals(CustomMetaDataStorageFilter.class.getName(), metaDataStorageFilters.get(0).getClass().getName());
    }

    public void testCustomMetaDataStorageFilterClassNameCustomConfig() throws Exception
    {
        DefaultMetaDataStorage metaDataStorage = new DefaultMetaDataStorage();
        // A little reflection stuff because filters can not be exposed in any
        // way.
        Field field = DefaultMetaDataStorage.class.getDeclaredField("metaDataStorageFilters");
        field.setAccessible(true);
        Object data = field.get(metaDataStorage);
        assertNotNull(data);
        List<MetaDataStorageFilter> metaDataStorageFilters = (List<MetaDataStorageFilter>) data;
        assertEquals(1, metaDataStorageFilters.size());
        assertEquals(Custom2MetaDataStorageFilter.class.getName(), metaDataStorageFilters.get(0).getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(ExtValCoreConfigurationCustomMetaDataStorageFilterClassNameTestCase.class);
    }

}
