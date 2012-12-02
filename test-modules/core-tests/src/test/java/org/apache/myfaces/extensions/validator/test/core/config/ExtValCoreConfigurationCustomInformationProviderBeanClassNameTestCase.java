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

import junit.framework.Test;

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.CustomInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.InformationProviderBean;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationCustomInformationProviderBeanClassNameTestCase extends
        ExtValCoreConfigurationTestCase
{

    public ExtValCoreConfigurationCustomInformationProviderBeanClassNameTestCase(String name)
    {
        super(name);

    }

    public static class CustomInformationProviderBean extends InformationProviderBean
    {

        @Override
        protected void applyCustomValues(Map<CustomInformation, String> map)
        {
            map.put(CustomInformation.MESSAGE_BUNDLE_NAME, "X");
        }

    }

    public static class CustomInformationProviderBean2 extends InformationProviderBean
    {

        @Override
        protected void applyCustomValues(Map<CustomInformation, String> map)
        {
            map.put(CustomInformation.MESSAGE_BUNDLE_NAME, "Y");
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_INFORMATION_PROVIDER_BEAN",
                    CustomInformationProviderBean.class.getName());
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
                public String customInformationProviderBeanClassName()
                {
                    return CustomInformationProviderBean2.class.getName();
                }

            };
        }
        else
        {
            return null;
        }

    }

    public void testCustomInformationProviderBeanClassNameDefault()
    {
        InformationProviderBean bean = ExtValContext.getContext().getInformationProviderBean();
        assertEquals(InformationProviderBean.class.getName(), bean.getClass().getName());
    }

    public void testCustomInformationProviderBeanClassNameWebXml()
    {
        InformationProviderBean bean = ExtValContext.getContext().getInformationProviderBean();
        assertEquals(CustomInformationProviderBean.class.getName(), bean.getClass().getName());
        // An additional test to make sure we have the custom
        // informationProviderBean.
        assertEquals(ExtValInformation.EXTENSIONS_VALIDATOR_BASE_PACKAGE_NAME + ".custom.X", bean
                .get(CustomInformation.MESSAGE_BUNDLE_NAME));
    }

    public void testCustomInformationProviderBeanClassNameCustomConfig()
    {
        InformationProviderBean bean = ExtValContext.getContext().getInformationProviderBean();
        assertEquals(CustomInformationProviderBean2.class.getName(), bean.getClass().getName());
        // An additional test to make sure we have the custom
        // informationProviderBean.
        assertEquals(ExtValInformation.EXTENSIONS_VALIDATOR_BASE_PACKAGE_NAME + ".custom.Y", bean
                .get(CustomInformation.MESSAGE_BUNDLE_NAME));
    }

    public static Test suite()
    {
        return new ClassLoaderTestSuite(ExtValCoreConfigurationCustomInformationProviderBeanClassNameTestCase.class);
    }

}
