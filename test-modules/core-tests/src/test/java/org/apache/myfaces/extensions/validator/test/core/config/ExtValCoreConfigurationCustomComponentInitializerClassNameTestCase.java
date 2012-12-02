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

import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import junit.framework.Test;

import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationCustomComponentInitializerClassNameTestCase extends ExtValCoreConfigurationTestCase
{

    public ExtValCoreConfigurationCustomComponentInitializerClassNameTestCase(String name)
    {
        super(name);
    }

    public static class CustomComponentInitializer implements ComponentInitializer
    {

        public void configureComponent(FacesContext facesContext, UIComponent uiComponent, Map<String, Object> metaData)
        {

        }

    }

    public static class Custom2ComponentInitializer implements ComponentInitializer
    {

        public void configureComponent(FacesContext facesContext, UIComponent uiComponent, Map<String, Object> metaData)
        {

        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_COMPONENT_INITIALIZER",
                    CustomComponentInitializer.class.getName());
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
                public String customComponentInitializerClassName()
                {
                    return Custom2ComponentInitializer.class.getName();
                }
            };
        }
        else
        {
            return null;
        }
    }

    public void testCustomComponentInitializerClassNameDefault()
    {
        assertEquals(0, ExtValContext.getContext().getComponentInitializers().size());
    }

    public void testCustomComponentInitializerClassNameWebXml()
    {
        List<ComponentInitializer> initializers = ExtValContext.getContext().getComponentInitializers();
        assertEquals(1, initializers.size());
        assertEquals(CustomComponentInitializer.class.getName(), initializers.get(0).getClass().getName());
    }

    public void testCustomComponentInitializerClassNameCustomConfig()
    {
        List<ComponentInitializer> initializers = ExtValContext.getContext().getComponentInitializers();
        assertEquals(1, initializers.size());
        assertEquals(Custom2ComponentInitializer.class.getName(), initializers.get(0).getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(ExtValCoreConfigurationCustomComponentInitializerClassNameTestCase.class);
    }

}
