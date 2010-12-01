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
package org.apache.myfaces.extensions.validator.test.trinidad;

import org.apache.myfaces.extensions.validator.core.renderkit.ExtValRenderKit;
import org.apache.myfaces.extensions.validator.test.base.AbstractExValTestCase;
import org.apache.myfaces.extensions.validator.trinidad.startup.TrinidadModuleStartupListener;
import org.apache.myfaces.test.config.ConfigParser;
import org.apache.myfaces.test.mock.MockRenderKit;
import org.apache.myfaces.trinidad.component.UIXInput;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.skin.SkinFactory;
import org.apache.myfaces.trinidadinternal.context.RequestContextBean;
import org.apache.myfaces.trinidadinternal.context.RequestContextImpl;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderKit;
import org.apache.myfaces.trinidadinternal.renderkit.core.CoreRenderingContext;
import org.apache.myfaces.trinidadinternal.skin.SkinFactoryImpl;

import javax.el.ValueExpression;
import javax.faces.FactoryFinder;
import javax.faces.el.ValueBinding;
import javax.faces.render.RenderKitFactory;
import java.net.URL;
import java.util.Enumeration;

/**
 * @author Rudy De Busscher
 *         since v4
 */
public abstract class AbstractTrinidadSupportTestCase extends AbstractExValTestCase
{

    private RequestContext requestContext;

    private RenderingContext renderingContext;

    @Override
    protected void setFactories() throws Exception
    {
        super.setFactories();
                FactoryFinder.setFactory(FactoryFinder.RENDER_KIT_FACTORY,
                "org.apache.myfaces.trinidadinternal.renderkit.CoreRenderKitFactory");

    }

    @Override
    protected void setUpRenderKit() throws Exception
    {

        RenderKitFactory renderKitFactory = (RenderKitFactory) FactoryFinder
                .getFactory(FactoryFinder.RENDER_KIT_FACTORY);
        renderKit = new ExtValRenderKit(new CoreRenderKit());
        renderKitFactory.addRenderKit(RenderKitFactory.HTML_BASIC_RENDER_KIT,
                renderKit);

        renderKitFactory.addRenderKit("org.apache.myfaces.trinidad.core",
                renderKit);
        renderKitFactory.addRenderKit("org.apache.myfaces.trinidadinternal.core",
                renderKit);
        renderKitFactory.addRenderKit("org.apache.myfaces.trinidad.core.desktop",
                renderKit);
        renderKitFactory.addRenderKit("org.apache.myfaces.trinidad.core.pda",
                renderKit);

        ConfigParser parser = new ConfigParser();

        Enumeration<URL> facesConfigEnum =getClass().getClassLoader().getResources("META-INF/faces-config.xml");

        while (facesConfigEnum.hasMoreElements() ) {
            URL url = facesConfigEnum.nextElement();
            if (url.toExternalForm().contains("trinidad")) {
                parser.parse(url);
            }
        }


    }

    @Override
    protected void invokeStartupListeners()
    {
        new TrinidadModuleStartupListener()
        {
            private static final long serialVersionUID = 423076920926752646L;

            @Override
            protected void init()
            {
                super.init();
            }
        }.init();

    }

    @Override
    protected void resetTestCase()
    {
        renderingContext.release();
        requestContext.release();
        super.resetTestCase();
    }

    @Override
    protected void setUpTestCase()
    {
        super.setUpTestCase();
        SkinFactory.setFactory(new SkinFactoryImpl());
        requestContext = new RequestContextImpl(new RequestContextBean());
        ((RequestContextImpl)requestContext).init(externalContext);
        renderingContext = new CoreRenderingContext();


    }

    protected void createValueBinding(UIXInput uiInput, String name, String expression)
    {
        ValueBinding valueBinding = application.createValueBinding(expression);
        ValueExpression valueExpression = application.getExpressionFactory().createValueExpression(
                facesContext.getELContext(), expression, Object.class);

        if (uiInput != null)
        {
            uiInput.setValueBinding(name, valueBinding);
            uiInput.setValueExpression(name, valueExpression);
        }
    }
}
