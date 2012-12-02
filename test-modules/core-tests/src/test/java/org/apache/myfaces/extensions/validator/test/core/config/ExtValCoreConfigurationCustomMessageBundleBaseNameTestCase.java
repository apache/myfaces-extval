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
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.DefaultValidationErrorMessageResolver;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationCustomMessageBundleBaseNameTestCase extends ExtValCoreConfigurationTestCase
{

    private static final String ORG_APACHE_MYFACES_EXTENSIONS_VALIDATOR_TEST = "org.apache.myfaces.extensions.validator.test.";
    private static final String ORG_APACHE_MYFACES_EXTENSIONS_VALIDATOR_CONFIG = "org.apache.myfaces.extensions.validator.config.";

    static class VisibleDefaultValidationErrorMessageResolver extends DefaultValidationErrorMessageResolver
    {
        public String getCustomBaseName()
        {
            return super.getCustomBaseName();

        }
    };

    public ExtValCoreConfigurationCustomMessageBundleBaseNameTestCase(String name)
    {
        super(name);
    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_MESSAGE_BUNDLE",
                    ORG_APACHE_MYFACES_EXTENSIONS_VALIDATOR_TEST);
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
                public String customMessageBundleBaseName()
                {

                    return ORG_APACHE_MYFACES_EXTENSIONS_VALIDATOR_CONFIG;
                }

            };
        }
        else
        {
            return null;
        }
    }

    public void testCustomMessageBundleBaseNameDefault() throws Exception
    {
        VisibleDefaultValidationErrorMessageResolver messageResolver = new VisibleDefaultValidationErrorMessageResolver();
        assertNull(messageResolver.getCustomBaseName());

    }

    public void testCustomMessageBundleBaseNameWebXml() throws Exception
    {

        VisibleDefaultValidationErrorMessageResolver messageResolver = new VisibleDefaultValidationErrorMessageResolver();

        assertEquals(ORG_APACHE_MYFACES_EXTENSIONS_VALIDATOR_TEST, messageResolver.getCustomBaseName());
    }

    public void testCustomMessageBundleBaseNameCustomConfig() throws Exception
    {
        VisibleDefaultValidationErrorMessageResolver messageResolver = new VisibleDefaultValidationErrorMessageResolver();

        assertEquals(ORG_APACHE_MYFACES_EXTENSIONS_VALIDATOR_CONFIG, messageResolver.getCustomBaseName());

    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(ExtValCoreConfigurationCustomMessageBundleBaseNameTestCase.class);
    }

}
