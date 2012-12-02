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

import javax.faces.context.FacesContext;
import javax.faces.render.RenderKit;
import javax.faces.render.Renderer;

import junit.framework.Test;

import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.renderkit.ExtValRendererProxy;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationRendererProxyTestCase extends ExtValCoreConfigurationTestCase
{

    private static final String WEB_XML = "Web.xml";
    private static final String CUSTOM_CONFIG = "Custom Config";

    public ExtValCoreConfigurationRendererProxyTestCase(String name)
    {
        super(name);
    }

    public static class CustomExtValRendererProxy extends ExtValRendererProxy
    {

        public CustomExtValRendererProxy(Renderer renderer)
        {
            super(renderer);
        }

        @Override
        public String convertClientId(FacesContext facesContext, String s)
        {
            return WEB_XML;
        }

    }

    public static class Custom2ExtValRendererProxy extends ExtValRendererProxy
    {

        public Custom2ExtValRendererProxy(Renderer renderer)
        {
            super(renderer);
        }

        @Override
        public String convertClientId(FacesContext facesContext, String s)
        {
            return CUSTOM_CONFIG;
        }

    }

    public static class CustomExtValCoreConfiguration extends DefaultExtValCoreConfiguration
    {

        public CustomExtValCoreConfiguration()
        {
            ExtValContext.getContext().addGlobalProperty(ExtValRendererProxy.KEY,
                    CustomExtValRendererProxy.class.getName(), true);
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValCoreConfiguration.class.getName(), CustomExtValCoreConfiguration.class.getName());
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
                public Class<? extends ExtValRendererProxy> rendererProxy()
                {
                    return Custom2ExtValRendererProxy.class;
                }
            };
        }
        else
        {
            return null;
        }

    }

    public void testRendererProxyDefault()
    {
        RenderKit kit = facesContext.getRenderKit();
        Renderer renderer = kit.getRenderer("javax.faces.Input", "javax.faces.Text");
        assertEquals("test", renderer.convertClientId(facesContext, "test"));
    }

    public void testRendererProxyWebXml()
    {
        RenderKit kit = facesContext.getRenderKit();
        Renderer renderer = kit.getRenderer("javax.faces.Input", "javax.faces.Text");
        assertEquals(WEB_XML, renderer.convertClientId(facesContext, "test"));
    }

    public void testRendererProxyCustomConfig()
    {
        RenderKit kit = facesContext.getRenderKit();
        Renderer renderer = kit.getRenderer("javax.faces.Input", "javax.faces.Text");
        assertEquals(CUSTOM_CONFIG, renderer.convertClientId(facesContext, "test"));
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(ExtValCoreConfigurationRendererProxyTestCase.class);
    }

}
