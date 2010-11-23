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

import java.util.List;

import junit.framework.Test;

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.interceptor.RendererInterceptor;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;
import org.apache.myfaces.extensions.validator.trinidad.DefaultExtValTrinidadSupportModuleConfiguration;
import org.apache.myfaces.extensions.validator.trinidad.ExtValTrinidadSupportModuleConfiguration;

/**
 * 
 * @author Rudy De Busscher
 * since v4
 *
 */
public class ExtValTrinidadSupportModuleConfigurationDeactivateCoreOutputLabelInitializationTestCase extends
        ExtValTrinidadSupportModuleConfigurationTestCase
{

    public ExtValTrinidadSupportModuleConfigurationDeactivateCoreOutputLabelInitializationTestCase(String name)
    {
        super(name);
    }

    public static class CustomExtValTrinidadSupportModuleConfiguration extends
            DefaultExtValTrinidadSupportModuleConfiguration
    {

        @Override
        public boolean deactivateCoreOutputLabelInitialization()
        {
            return true;
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".DEACTIVATE_TRINIDAD_CORE_OUTPUT_LABEL_INITIALIZATION", "true");

        }
    }

    @Override
    protected ExtValTrinidadSupportModuleConfiguration getCustomTrinidadSupportModuleConfiguration()
    {
        return new CustomExtValTrinidadSupportModuleConfiguration();
    }

    public void testDeactivateCoreOutputLabelInitializationDefault()
    {
        List<RendererInterceptor> interceptors = ExtValContext.getContext().getRendererInterceptors();
        // Default ValidationInterceptor from core and Trinidad one.
        assertEquals(2, interceptors.size());
    }

    public void testDeactivateCoreOutputLabelInitializationWebXml()
    {
        List<RendererInterceptor> interceptors = ExtValContext.getContext().getRendererInterceptors();
        // only Default ValidationInterceptor from core .
        assertEquals(1, interceptors.size());
    }

    public void testDeactivateCoreOutputLabelInitializationCustomConfig()
    {
        List<RendererInterceptor> interceptors = ExtValContext.getContext().getRendererInterceptors();
        // only Default ValidationInterceptor from core .
        assertEquals(1, interceptors.size());
    }

    public static Test suite()
    {
        return new ClassLoaderTestSuite(
                ExtValTrinidadSupportModuleConfigurationDeactivateCoreOutputLabelInitializationTestCase.class);

    }

}
