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

import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ViolationSeverity;
import org.junit.Assert;
import org.junit.Test;


/**
 * 
 * @author Rudy De Busscher
 * since v4
 *
 */
public class ExtValCoreConfigurationCustomTestCase extends ExtValCoreConfigurationTestCase
{

    // Testcase to see if the configuration object from web.xml is taken.
    // Basicly a test for ExtValContext.addModuleConfiguration

    
    public static class CustomExtValCoreConfiguration extends DefaultExtValCoreConfiguration {

        @Override
        public Class violationSeverity()
        {
            return Object.class;
        }
        
    }
    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needCustomConfig())
        {
            addInitParameter(ExtValCoreConfiguration.class.getName(),
                    CustomExtValCoreConfiguration.class.getName());

        }
    }
    @Override
    protected ExtValCoreConfiguration getCustomExtValCoreConfiguration()
    {
        // Don't specify the custom config here.  We explicitly want to test the web.xml parameter.
        return null;
    }
    

    @Test
    public void testExtValCoreConfigurationCustomDefault()
    {
        Assert.assertEquals(ViolationSeverity.class.getName(), ((Class)ExtValCoreConfiguration.get().violationSeverity()).getName());
    }

    @Test
    public void testExtValCoreConfigurationCustomCustomConfig()
    {
        Assert.assertEquals(Object.class.getName(), ((Class)ExtValCoreConfiguration.get().violationSeverity()).getName());
    }
}
