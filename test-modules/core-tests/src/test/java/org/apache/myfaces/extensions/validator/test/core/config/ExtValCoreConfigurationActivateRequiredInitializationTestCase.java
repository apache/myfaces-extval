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
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

/**
 * 
 * @author Rudy De Busscher
 * since v4
 *
 */
public class ExtValCoreConfigurationActivateRequiredInitializationTestCase extends ExtValCoreConfigurationTestCase
{

    public ExtValCoreConfigurationActivateRequiredInitializationTestCase(String name)
    {
        super(name);
    }

    @Override
    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".ACTIVATE_REQUIRED_INITIALIZATION", "true");
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
                public boolean activateRequiredInitialization()
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

    public void testActivateRequiredInitializationDefault()
    {
        assertFalse(ExtValUtils.isRequiredInitializationActive());
    }

    public void testActivateRequiredInitializationDefaultWithOverrule()
    {
        DefaultExtValCoreConfiguration.overruleActivateRequiredInitialization(Boolean.TRUE, true);
        assertTrue(ExtValUtils.isRequiredInitializationActive());
    }

    public void testActivateRequiredInitializationWebXml()
    {
        assertTrue(ExtValUtils.isRequiredInitializationActive());
    }

    public void testActivateRequiredInitializationWebXmlWithOverrule()
    {
        DefaultExtValCoreConfiguration.overruleActivateRequiredInitialization(Boolean.FALSE, true);
        assertFalse(ExtValUtils.isRequiredInitializationActive());
    }

    public void testActivateRequiredInitializationCustomConfig()
    {
        assertTrue(ExtValUtils.isRequiredInitializationActive());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(ExtValCoreConfigurationActivateRequiredInitializationTestCase.class);
    }
}
