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

import junit.framework.TestCase;
import org.apache.shale.test.mock.*;
import org.apache.shale.test.el.MockExpressionFactory;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.test.base.mock.*;
import org.apache.myfaces.extensions.validator.test.base.util.TestUtils;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.core.renderkit.DefaultRenderKitWrapperFactory;
import org.apache.myfaces.extensions.validator.core.startup.ExtValStartupListener;
import org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ProjectStageResolver;
import org.apache.myfaces.extensions.validator.core.el.AbstractELHelperFactory;
import org.apache.myfaces.extensions.validator.core.el.ELHelper;
import org.apache.myfaces.shared_impl.config.MyfacesConfig;

import javax.faces.render.RenderKit;
import javax.faces.render.RenderKitFactory;
import javax.faces.FactoryFinder;
import javax.faces.el.ValueBinding;
import javax.faces.application.ApplicationFactory;
import javax.faces.application.FacesMessage;
import javax.faces.component.UIViewRoot;
import javax.faces.component.UIInput;
import javax.el.ValueExpression;
import java.net.URLClassLoader;
import java.net.URL;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Iterator;

/**
 * Abstract Shale Test base class, which sets up the JSF environment.  If the test
 * overrides <code>setUp()</code> and/or <code>tearDown()</code>, then those
 * methods but call the overwitten method to insure a valid test environment.
 */
@SuppressWarnings("deprecation")
public abstract class AbstractExValTestCase extends TestCase
{
    protected MockApplication application;
    protected MockServletConfig config;
    protected MockExternalContext externalContext;
    protected MockFacesContext facesContext;
    protected MockFacesContextFactory facesContextFactory;
    protected MockLifecycle lifecycle;
    protected MockLifecycleFactory lifecycleFactory;
    protected RenderKit renderKit;
    protected MockHttpServletRequest request;
    protected MockHttpServletResponse response;
    protected MockServletContext servletContext;
    protected MockHttpSession session;
    protected MockExpressionFactory expressionFactory;
    private ClassLoader threadContextClassLoader;

    /** Response Writer */
    private MockResponseWriter writer;

    /**
     * Construct a new instance of the test.
     *
     * @param name Name of the test.
     */
    public AbstractExValTestCase(String name)
    {
        super(name);
        application = null;
        config = null;
        externalContext = null;
        facesContext = null;
        facesContextFactory = null;
        lifecycle = null;
        lifecycleFactory = null;
        renderKit = null;
        request = null;
        response = null;
        servletContext = null;
        session = null;
        expressionFactory = null;
        threadContextClassLoader = null;
    }

    /**
     *  Setup the test environment, including the following:
     *  <ul>
     *  <li>Set the Application Map.</li>
     *  <li>Set a response writer</li>
     *  <li>Add Tomahawk's renderers to the faces context.</li>
     *  </ul>
     */
    protected void setUp() throws Exception
    {
        threadContextClassLoader = Thread.currentThread().getContextClassLoader();
        Thread.currentThread().setContextClassLoader(new URLClassLoader(new URL[0], getClass().getClassLoader()));
        servletContext = new MockServletContext();
        //for testing the fallback
        //servletContext.addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".DEACTIVATE_EL_RESOLVER", "true");
        servletContext.addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_VALIDATION_STRATEGY_FACTORY", MockValidationStrategyFactory.class.getName());
        servletContext.addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_MESSAGE_RESOLVER_FACTORY", MockMessageResolverFactory.class.getName());
        servletContext.addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX + ".CUSTOM_META_DATA_TRANSFORMER_FACTORY", MockMetaDataTransformerFactory.class.getName());
        config = new MockServletConfig(servletContext);
        session = new MockHttpSession();
        session.setServletContext(servletContext);
        request = new MockHttpServletRequest(session);
        request.setServletContext(servletContext);
        response = new MockHttpServletResponse();
        FactoryFinder.releaseFactories();
        FactoryFinder.setFactory("javax.faces.application.ApplicationFactory", ExtValMockApplicationFactory.class.getName());
        FactoryFinder.setFactory("javax.faces.context.FacesContextFactory", "org.apache.shale.test.mock.MockFacesContextFactory");
        FactoryFinder.setFactory("javax.faces.lifecycle.LifecycleFactory", "org.apache.shale.test.mock.MockLifecycleFactory");
        FactoryFinder.setFactory("javax.faces.render.RenderKitFactory", "org.apache.shale.test.mock.MockRenderKitFactory");
        externalContext = new MockExternalContext(servletContext, request, response);
        lifecycleFactory = (MockLifecycleFactory)FactoryFinder.getFactory("javax.faces.lifecycle.LifecycleFactory");
        lifecycle = (MockLifecycle)lifecycleFactory.getLifecycle("DEFAULT");
        facesContextFactory = (MockFacesContextFactory)FactoryFinder.getFactory("javax.faces.context.FacesContextFactory");
        facesContext = (MockFacesContext)facesContextFactory.getFacesContext(servletContext, request, response, lifecycle);
        externalContext = (MockExternalContext)facesContext.getExternalContext();
        UIViewRoot root = new UIViewRoot();
        root.setViewId("/viewId");
        root.setRenderKitId("HTML_BASIC");
        facesContext.setViewRoot(root);
        ApplicationFactory applicationFactory = (ApplicationFactory)FactoryFinder.getFactory("javax.faces.application.ApplicationFactory");
        application = (MockApplication)applicationFactory.getApplication();
        facesContext.setApplication(application);
        RenderKitFactory renderKitFactory = (RenderKitFactory)FactoryFinder.getFactory("javax.faces.render.RenderKitFactory");
        //Wrap renderers with proper exval wrapper
        renderKit = new DefaultRenderKitWrapperFactory().create(new MockRenderKit());
        renderKitFactory.addRenderKit("HTML_BASIC", renderKit);

        // additional setup not provided automatically by the shale mock stuff
        facesContext.getExternalContext().getApplicationMap().put(MyfacesConfig.class.getName(), new MyfacesConfig());
        writer = new MockResponseWriter(new StringWriter(), null, null);
        facesContext.setResponseWriter(writer);

        TestUtils.addDefaultRenderers(facesContext);
        TestUtils.addDefaultValidators(facesContext);

        expressionFactory = (MockExpressionFactory)application.getExpressionFactory();

        final ELHelper defaultElHelper = ExtValUtils.getELHelper();
        ExtValContext.getContext().getFactoryFinder()
                .getFactory(FactoryNames.EL_HELPER_FACTORY, AbstractELHelperFactory.class)
                .setCustomELHelperFactory(new AbstractELHelperFactory() {

                    protected ELHelper createELHelper()
                    {
                        return new MockELHelper(defaultElHelper);
                    }
                });

        //execute startup listener
        new ExtValStartupListener() {
            private static final long serialVersionUID = -3861810605160281884L;

            @Override
            protected void init()
            {
                ExtValContext.getContext()
                        .addGlobalProperty(ProjectStageResolver.class.getName(), getProjectStageResolver(), false);
                super.init();
            }
        }.init();

        invokeStartupListeners();
    }

    protected abstract void invokeStartupListeners();

    /**
     * Tear down the test environment.
     */
    protected void tearDown() throws Exception
    {
        resetFactoryFinder();
        resetExtValContext();

        application = null;
        config = null;
        externalContext = null;
        facesContext.release();
        facesContext = null;
        lifecycle = null;
        lifecycleFactory = null;
        renderKit = null;
        request = null;
        response = null;
        servletContext = null;
        session = null;
        FactoryFinder.releaseFactories();
        Thread.currentThread().setContextClassLoader(threadContextClassLoader);
        expressionFactory = null;
        threadContextClassLoader = null;
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

    protected void checkMessageCount(int expected)
    {
        int i = 0;
        for(Iterator messages = facesContext.getMessages(); messages.hasNext();)
        {
            messages.next();
            i++;
        }

        assertEquals("Complete message count", expected, i);
    }

    protected void checkMessageCount(String clientId, int expected)
    {
        int i = 0;
        for(Iterator messages = facesContext.getMessages(clientId); messages.hasNext();)
        {
            messages.next();
            i++;
        }

        assertEquals("Complete message count", expected, i);
    }

    protected void checkMessageSeverities(FacesMessage.Severity... severities)
    {
        int i = 0;
        for(Iterator messages = facesContext.getMessages(); messages.hasNext();)
        {
            assertEquals(severities[i], ((FacesMessage)messages.next()).getSeverity());
            i++;
        }
    }

    protected void assertNavigationBlocked(boolean isBlocked)
    {
        assertEquals(isBlocked, this.facesContext.getRenderResponse());
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
        assertNotNull("ID is not null", id);
        assertTrue("Response Complete", facesContext.getResponseComplete());
        String output = writer.getWriter().toString();
        assertNotNull("Has output", output);
        assertTrue("Contains id '" + id + "'", output.indexOf(id) != -1);
    }

    protected void createValueBinding(UIInput uiInput, String name, String expression)
    {
        ValueBinding valueBinding = application.createValueBinding(expression);
        ValueExpression valueExpression = expressionFactory.createValueExpression(
            facesContext.getELContext(), expression, Object.class);

        if(uiInput != null)
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
}

