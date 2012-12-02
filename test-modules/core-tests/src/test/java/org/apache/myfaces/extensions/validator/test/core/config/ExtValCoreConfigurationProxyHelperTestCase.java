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
import org.apache.myfaces.extensions.validator.core.proxy.ProxyHelper;
import org.apache.myfaces.extensions.validator.util.ProxyUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationProxyHelperTestCase extends ExtValCoreConfigurationTestCase
{
    public static class CustomProxyHelper implements ProxyHelper
    {

        public String getClassNameOfObject(Object proxiedObject)
        {
            return null;
        }

        public String getNameOfClass(Class proxiedClass)
        {
            return null;
        }

        public Class getUnproxiedClass(Class currentClass)
        {
            return null;
        }

        public <T> Class<T> getUnproxiedClass(Class currentClass, Class<T> targetType)
        {
            return null;
        }

        public boolean isProxiedClass(Class currentClass)
        {
            if (currentClass.equals(Object.class))
            {
                return true;
            }
            return false;
        }

        public boolean isProxiedObject(Object proxiedObject)
        {
            return false;
        }

    }

    protected void setUpTestCase()
    {

        // Trick the method jsfUtils#isApplicationInitialized to believe the
        // application is initialized
        facesContext.getExternalContext().getRequestMap().put("Key", "Value");
    }

    @Override
    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_PROXY_HELPER", CustomProxyHelper.class
                    .getName());
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
                public ProxyHelper proxyHelper()
                {
                    return new CustomProxyHelper();
                }
            };
        }
        else
        {
            return null;
        }
    }

    @Test
    public void testProxyHelperDefault()
    {
        Assert.assertFalse(ProxyUtils.isProxiedClass(Object.class));
    }

    @Test
    public void testProxyHelperWebXml()
    {
        Assert.assertTrue(ProxyUtils.isProxiedClass(Object.class));
    }

    @Test
    public void testProxyHelperCustomConfig()
    {
        Assert.assertTrue(ProxyUtils.isProxiedClass(Object.class));
    }


}
