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
package org.apache.myfaces.extensions.validator.test.trinidad.config;

import junit.framework.Test;

import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;
import org.apache.myfaces.extensions.validator.trinidad.DefaultExtValTrinidadSupportModuleConfiguration;
import org.apache.myfaces.extensions.validator.trinidad.ExtValTrinidadSupportModuleConfiguration;

/**
 * 
 * @author Rudy De Busscher
 * since v4
 *
 */
public class ExtValTrinidadSupportModuleConfigurationCustomTestCase extends
        ExtValTrinidadSupportModuleConfigurationTestCase
{

    public ExtValTrinidadSupportModuleConfigurationCustomTestCase(String name)
    {
        super(name);
    }

    public static class CustomExtValTrinidadSupportModuleConfiguration extends
            DefaultExtValTrinidadSupportModuleConfiguration
    {

        @Override
        public boolean deactivateClientSideValidation()
        {
            return true;
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needCustomConfig())
        {
            addInitParameter(ExtValTrinidadSupportModuleConfiguration.class.getName(),
                    CustomExtValTrinidadSupportModuleConfiguration.class.getName());

        }
    }

    @Override
    protected ExtValTrinidadSupportModuleConfiguration getCustomTrinidadSupportModuleConfiguration()
    {
        // Don't specify the custom config here. We explicitly want to test the
        // web.xml parameter.
        return null;
    }

    public void testExtValTrinidadSupportModuleConfigurationCustomDefault()
    {
        assertFalse(ExtValTrinidadSupportModuleConfiguration.get().deactivateClientSideValidation());
    }

    public void testExtValTrinidadSupportModuleConfigurationCustomCustomConfig()
    {
        assertTrue(ExtValTrinidadSupportModuleConfiguration.get().deactivateClientSideValidation());

    }

    public static Test suite()
    {
        return new ClassLoaderTestSuite(ExtValTrinidadSupportModuleConfigurationCustomTestCase.class);
    }

}
