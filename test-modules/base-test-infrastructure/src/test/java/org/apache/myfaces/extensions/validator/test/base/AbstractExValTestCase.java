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
package org.apache.myfaces.extensions.validator.test.base;

import org.apache.myfaces.extensions.validator.core.renderkit.ExtValRenderKit;
import org.apache.myfaces.test.base.junit4.AbstractJsfConfigurableMockTestCase;
import org.apache.myfaces.test.config.ConfigParser;
import org.apache.myfaces.test.el.MockExpressionFactory;
import org.apache.myfaces.test.mock.MockRenderKit;
import org.apache.myfaces.test.mock.MockResponseWriter;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.test.base.mock.*;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.core.startup.ExtValStartupListener;
import org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValModuleConfiguration;
import org.apache.myfaces.extensions.validator.core.el.AbstractELHelperFactory;
import org.apache.myfaces.extensions.validator.core.el.ELHelper;

import org.junit.After;
import org.junit.Assert;

import javax.faces.FactoryFinder;
import javax.faces.el.ValueBinding;
import javax.faces.application.FacesMessage;
import javax.faces.component.UIInput;
import javax.el.ValueExpression;
import javax.faces.render.RenderKitFactory;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Iterator;

/**
 * Abstract ExtVal Test base class, which sets up the JSF environment.  If the test
 * overrides any of the <code>setUp()</code> and/or <code>tearDown()</code>, related methods,  then those
 * methods need to call the overwitten method to insure a valid test environment.
 */
@SuppressWarnings("deprecation")
public abstract class AbstractExValTestCase extends AbstractJsfConfigurableMockTestCase
{

    private MockResponseWriter writer;

    private StringWriter page;

    private MockExpressionFactory expressionFactory;

    // Setting up the JSF System

    @Override
    protected void setUpServletObjects() throws Exception
    {
        super.setUpServletObjects();

        //servletContext.addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".DEACTIVATE_EL_RESOLVER", "true");
        servletContext.addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_VALIDATION_STRATEGY_FACTORY", MockValidationStrategyFactory.class.getName());
        servletContext.addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_MESSAGE_RESOLVER_FACTORY", MockMessageResolverFactory.class.getName());
        servletContext.addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_META_DATA_TRANSFORMER_FACTORY", MockMetaDataTransformerFactory.class.getName());

        addInitializationParameters();

    }

    @Override
    protected void setUpRenderKit() throws Exception
    {
        // Don't call super since we need the ExtValRenderKit and not the Mock one.

        RenderKitFactory renderKitFactory = (RenderKitFactory) FactoryFinder
                .getFactory(FactoryFinder.RENDER_KIT_FACTORY);
        renderKit = new ExtValRenderKit(new MockRenderKit());
        renderKitFactory.addRenderKit(RenderKitFactory.HTML_BASIC_RENDER_KIT,
                renderKit);

        // Initialize complete JSF system
        ConfigParser parser = new ConfigParser();
        parser.parse(parser.getPlatformURLs());

    }

    @Override
    protected void setUpFacesContext() throws Exception
    {
        super.setUpFacesContext();

        page = new StringWriter();
        writer = new MockResponseWriter(page, null, null);
        facesContext.setResponseWriter(writer);

    }

    @Override
    protected void setUpApplication() throws Exception
    {
        super.setUpApplication();

        externalContext.getApplicationMap().put("org.apache.myfaces.application.ApplicationImpl", application);

        expressionFactory = (MockExpressionFactory) application.getExpressionFactory();
    }

    /**
     * Setup the test environment, including the following:
     * <ul>
     * <li>Set the Application Map.</li>
     * <li>Set a response writer</li>
     * <li>Add Tomahawk's renderers to the faces context.</li>
     * </ul>
     */
    public void setUp() throws Exception
    {
        super.setUp();

        applyCustomConfigurations();

        final ExtValCoreConfiguration customExtValCoreConfiguration = getCustomExtValCoreConfiguration();

        //execute startup listener
        new ExtValStartupListener()
        {
            private static final long serialVersionUID = -3861810605160281884L;

            @Override
            protected void initModuleConfig()
            {
                if (customExtValCoreConfiguration != null)
                {
                    ExtValCoreConfiguration.use(customExtValCoreConfiguration, true);
                }
                else
                {
                    ExtValCoreConfiguration.use(new DefaultExtValCoreConfiguration(), true);
                }
            }

            @Override
            protected void init()
            {
                initModuleConfig();
                super.init();
            }
        }.init();

        invokeStartupListeners();

        setUpTestCase();
    }

    // Customizations of the setup.


    protected void setUpTestCase()
    {
        //override it - if needed
    }

    protected ExtValCoreConfiguration getCustomExtValCoreConfiguration()
    {
        //override it - if needed
        return null;
    }

    protected void addInitializationParameters()
    {
        //override it - if needed
    }

    protected abstract void invokeStartupListeners();

    protected ExtValModuleConfiguration[] getCustomConfigObjects()
    {
        return null;
    }

    protected void addInitParameter(String key, String value)
    {
        servletContext.addInitParameter(key, value);
    }

    // Cleaning up after a test

    @After
    public void reset()
    {
        resetFactoryFinder();
        resetExtValContext();
        resetTestCase();
    }

    protected void resetTestCase()
    {
        // Override if needed
    }

    private void resetFactoryFinder()
    {
        try
        {
            Field factoryMap = DefaultFactoryFinder.class.getDeclaredField("factoryMap");
            factoryMap.setAccessible(true);
            factoryMap.set(ExtValContext.getContext().getFactoryFinder(), new HashMap<FactoryNames, Object>());
        }
        catch (Exception e)
        {
            throw new IllegalStateException("cannot reset the factory finder", e);
        }
    }

    protected void resetExtValContext()
    {
        try
        {
            Field context = ExtValContext.class.getDeclaredField("extValContext");
            context.setAccessible(true);
            context.set(ExtValContext.getContext(), null);
        }
        catch (Exception e)
        {
            throw new IllegalStateException("cannot reset the extval-context", e);
        }
    }

    // General utility methods for tests

    protected void checkMessageCount(int expected)
    {
        int i = 0;
        for (Iterator messages = facesContext.getMessages(); messages.hasNext();)
        {
            messages.next();
            i++;
        }

        Assert.assertEquals("Complete message count", expected, i);
    }

    protected void checkMessageCount(String clientId, int expected)
    {
        int i = 0;
        for (Iterator messages = facesContext.getMessages(clientId); messages.hasNext();)
        {
            messages.next();
            i++;
        }

        Assert.assertEquals("Complete message count", expected, i);
    }

    protected void checkMessageSeverities(FacesMessage.Severity... severities)
    {
        int i = 0;
        for (Iterator messages = facesContext.getMessages(); messages.hasNext();)
        {
            Assert.assertEquals(severities[i], ((FacesMessage) messages.next()).getSeverity());
            i++;
        }
    }

    protected void assertNavigationBlocked(boolean isBlocked)
    {
        Assert.assertEquals(isBlocked, this.facesContext.getRenderResponse());
    }

    /**
     * Verify the following:
     * <ul>
     * <li>id is not null</li>
     * <li>Response is complete</li>
     * <li>Responce contains the id</li>
     * </ul>
     *
     * @param id ID to verify
     */
    protected void assertIdExists(String id)
    {
        Assert.assertNotNull("ID is not null", id);
        Assert.assertTrue("Response Complete", facesContext.getResponseComplete());
        String output = writer.getWriter().toString();
        Assert.assertNotNull("Has output", output);
        Assert.assertTrue("Contains id '" + id + "'", output.indexOf(id) != -1);
    }

    // Handy methods for within tests

    protected void createValueBinding(UIInput uiInput, String name, String expression)
    {
        ValueBinding valueBinding = application.createValueBinding(expression);
        ValueExpression valueExpression = expressionFactory.createValueExpression(
                facesContext.getELContext(), expression, Object.class);

        if (uiInput != null)
        {
            uiInput.setValueBinding(name, valueBinding);
            uiInput.setValueExpression(name, valueExpression);
        }
    }

    protected void createRequestScopedBean(String name, Object instance)
    {
        createValueBinding(null, "value", "#{" + name + "}");
        facesContext.getExternalContext().getRequestMap().put(name, instance);
    }

    @SuppressWarnings({"UnusedDeclaration", "unchecked"})
    protected <T> T resolveBean(String name, Class<T> targetClass)
    {
        return (T) ExtValUtils.getELHelper().getBean(name);
    }

    private void applyCustomConfigurations()
    {
        ExtValModuleConfiguration[] customConfigs = getCustomConfigObjects();
        if (customConfigs != null)
        {
            for (ExtValModuleConfiguration moduleConfiguration : customConfigs)
            {
                ExtValContext.getContext().addModuleConfiguration(getAbstractModuleConfiguration(moduleConfiguration),
                        moduleConfiguration, false);
            }
        }
    }

    @SuppressWarnings("unchecked")
    private static Class<? extends ExtValModuleConfiguration> getAbstractModuleConfiguration(
            Object moduleConfiguration)
    {
        Class<? extends ExtValModuleConfiguration> result = (Class<? extends ExtValModuleConfiguration>) moduleConfiguration
                .getClass();
        while (!Modifier.isAbstract(result.getModifiers())
                && !Object.class.getName().equals(result.getName()))
        {
            result = (Class<? extends ExtValModuleConfiguration>) result
                    .getSuperclass();
        }
        return result;
    }
}

