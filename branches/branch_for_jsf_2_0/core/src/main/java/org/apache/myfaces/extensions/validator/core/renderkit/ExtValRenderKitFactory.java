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
package org.apache.myfaces.extensions.validator.core.renderkit;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.render.RenderKitFactory;
import javax.faces.render.RenderKit;
import javax.faces.context.FacesContext;
import java.util.Iterator;

/**
 * central mechanism which is responsible to create a wrapper for a renderer - starting point of extval.
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ExtValRenderKitFactory extends RenderKitFactory
{
    protected final Log logger = LogFactory.getLog(getClass());
    private RenderKitFactory wrapped;
    private AbstractRenderKitWrapperFactory defaultRenderKitWrapperFactory;
    private Boolean isDeactivated;

    public ExtValRenderKitFactory(RenderKitFactory renderKitFactory)
    {
        this.wrapped = renderKitFactory;

        if(logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }
    }

    public void addRenderKit(String s, RenderKit renderKit)
    {
        this.wrapped.addRenderKit(s, renderKit);
    }

    public RenderKit getRenderKit(FacesContext facesContext, String s)
    {
        RenderKit renderKit = this.wrapped.getRenderKit(facesContext, s);

        //jsf ri + trinidad
        if(renderKit == null)
        {
            return null;
        }

        tryToInitDefaultRenderKitWrapperFactory();

        checkRenderKitFactoryDeactivation();

        if(this.isDeactivated)
        {
            return renderKit;
        }

        //test early config in case of mojarra
        if(!this.defaultRenderKitWrapperFactory.isApplicationInitialized())
        {
            return this.defaultRenderKitWrapperFactory.createWrapper(renderKit);
        }

        return tryToCreateWrapperWithWrapperFactory(renderKit);
    }

    private void checkRenderKitFactoryDeactivation()
    {
        if(this.isDeactivated == null)
        {
            if(this.defaultRenderKitWrapperFactory.isApplicationInitialized())
            {
                this.isDeactivated = isRenderKitFactoryDeactivatedViaWebXml();
            }
            else
            {
                this.isDeactivated = isRenderKitFactoryDeactivatedViaVMParameter();
            }
        }
    }

    private synchronized void tryToInitDefaultRenderKitWrapperFactory()
    {
        if(this.defaultRenderKitWrapperFactory == null)
        {
            //workaround for mojarra to allow a custom factory during the early config phase
            //just create the factory with the given name
            //+it should extend your custom implementation which you register as usual
            Object customFactory = ClassUtils.tryToInstantiateClassForName(
                    ExtValInformation.EXTENSIONS_VALIDATOR_BASE_PACKAGE_NAME + ".custom.RenderKitWrapperFactory");

            if(customFactory instanceof AbstractRenderKitWrapperFactory)
            {
                this.defaultRenderKitWrapperFactory = (AbstractRenderKitWrapperFactory)customFactory;
            }
            else
            {
                this.defaultRenderKitWrapperFactory = new DefaultRenderKitWrapperFactory();
            }
        }
    }

    private RenderKit tryToCreateWrapperWithWrapperFactory(RenderKit renderKit)
    {
        AbstractRenderKitWrapperFactory wrapperFactory = ExtValContext.getContext().getFactoryFinder()
            .getFactory(FactoryNames.RENDERKIT_WRAPPER_FACTORY, AbstractRenderKitWrapperFactory.class);

        //some component libs e.g. myfaces-trinidad aren't compatible with this clean approach
        //example see TrinidadModuleStartupListener
        if(wrapperFactory.isDeactivated())
        {
            return renderKit;
        }

        return wrapperFactory.create(renderKit);
    }

    public Iterator<String> getRenderKitIds()
    {
        return this.wrapped.getRenderKitIds();
    }

    private boolean isRenderKitFactoryDeactivatedViaWebXml()
    {
        return "true".equalsIgnoreCase(WebXmlParameter.DEACTIVATE_RENDER_KIT_FACTORY);
    }

    private boolean isRenderKitFactoryDeactivatedViaVMParameter()
    {
        return "true".equalsIgnoreCase(System
                .getProperty(ExtValInformation.EXTENSIONS_VALIDATOR_BASE_PACKAGE_NAME +
                    ".DEACTIVATE_RENDER_KIT_FACTORY", "false"));
    }
}
