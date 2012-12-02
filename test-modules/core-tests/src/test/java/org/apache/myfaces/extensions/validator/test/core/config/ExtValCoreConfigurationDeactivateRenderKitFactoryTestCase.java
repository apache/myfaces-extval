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

import javax.faces.render.RenderKit;
import javax.faces.render.RenderKitFactory;

import junit.framework.Test;

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.renderkit.ExtValRenderKit;
import org.apache.myfaces.extensions.validator.core.renderkit.ExtValRenderKitFactory;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;
import org.apache.shale.test.mock.MockRenderKit;
import org.apache.shale.test.mock.MockRenderKitFactory;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationDeactivateRenderKitFactoryTestCase extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationDeactivateRenderKitFactoryTestCase(String name)
    {
        super(name);
    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".DEACTIVATE_RENDER_KIT_FACTORY", "true");
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
                public boolean deactivateRenderKitFactory()
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

    public void testDeactivateRenderKitFactoryDefault()
    {
        RenderKitFactory factory = new ExtValRenderKitFactory(new MockRenderKitFactory());
        RenderKit renderKit = factory.getRenderKit(facesContext, RenderKitFactory.HTML_BASIC_RENDER_KIT);
        assertEquals(ExtValRenderKit.class.getName(), renderKit.getClass().getName());

    }

    public void testDeactivateRenderKitFactoryWebXml()
    {
        RenderKitFactory factory = new ExtValRenderKitFactory(new MockRenderKitFactory());
        RenderKit renderKit = factory.getRenderKit(facesContext, RenderKitFactory.HTML_BASIC_RENDER_KIT);
        assertEquals(MockRenderKit.class.getName(), renderKit.getClass().getName());
    }

    public void testDeactivateRenderKitFactoryCustomConfig()
    {
        RenderKitFactory factory = new ExtValRenderKitFactory(new MockRenderKitFactory());
        RenderKit renderKit = factory.getRenderKit(facesContext, RenderKitFactory.HTML_BASIC_RENDER_KIT);
        assertEquals(MockRenderKit.class.getName(), renderKit.getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(ExtValCoreConfigurationDeactivateRenderKitFactoryTestCase.class);
    }

}
