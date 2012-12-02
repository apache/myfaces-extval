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
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationDeactivateDefaultConventionTestCase extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationDeactivateDefaultConventionTestCase(String name)
    {
        super(name);
    }

    @Override
    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".DEACTIVATE_DEFAULT_CONVENTION", "true");
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
                public boolean deactivateDefaultConvention()
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

    @ToDo(value = Priority.MEDIUM, description = "some better logic to see if it is really well integrated")
    public void testDeactivateDefConventionDefault() throws Exception
    {
        // Not a very clever testcase but code in
        // AbstractValidationErrorMessageResolver is too complex to have a quick
        // simple test of the parameter
        assertFalse(ExtValCoreConfiguration.get().deactivateDefaultConvention());
    }

    // Name of method should contain default
    public void testDeactivateDefConventionWebXml()
    {
        assertTrue(ExtValCoreConfiguration.get().deactivateDefaultConvention());
    }

    // Name of method should contain default
    public void testDeactivateDefConventionCustomConfig()
    {
        assertTrue(ExtValCoreConfiguration.get().deactivateDefaultConvention());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(ExtValCoreConfigurationDeactivateDefaultConventionTestCase.class);
    }

}
