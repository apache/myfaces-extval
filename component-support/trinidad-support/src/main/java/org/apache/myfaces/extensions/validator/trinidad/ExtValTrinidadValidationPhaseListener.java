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
package org.apache.myfaces.extensions.validator.trinidad;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.trinidad.renderkit.ExtValTrinidadRenderKit;

import javax.faces.FactoryFinder;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;
import javax.faces.render.RenderKit;
import javax.faces.render.RenderKitFactory;

/**
 * use this phase-listener instead of a ExtValTrinidadRenderKitWrapperFactory due to a trinidad issue
 *
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ExtValTrinidadValidationPhaseListener implements PhaseListener
{
    private static final long serialVersionUID = 9109721831864146165L;

    public void afterPhase(PhaseEvent event)
    {
    }

    public void beforePhase(PhaseEvent event)
    {
        FacesContext facesContext = FacesContext.getCurrentInstance();

        RenderKitFactory renderKitFactory = getRenderKitFactory();

        String renderKitId = getRenderKitId(facesContext);

        if (isIncompatibleRenderKit(renderKitId))
        {
            changeRenderKit(facesContext, renderKitFactory, renderKitId);
        }
    }

    private String getRenderKitId(FacesContext facesContext)
    {
        return facesContext.getViewRoot().getRenderKitId();
    }

    private RenderKitFactory getRenderKitFactory()
    {
        return (RenderKitFactory)FactoryFinder.getFactory(FactoryFinder.RENDER_KIT_FACTORY);
    }

    private boolean isIncompatibleRenderKit(String renderKitId)
    {
        return !ExtValTrinidadRenderKit.ID.equals(renderKitId);
    }

    private void changeRenderKit(FacesContext facesContext, RenderKitFactory renderKitFactory, String renderKitId)
    {
        RenderKit renderKit = renderKitFactory.getRenderKit(FacesContext.getCurrentInstance(), renderKitId);
        renderKitFactory.addRenderKit(ExtValTrinidadRenderKit.ID, new ExtValTrinidadRenderKit(renderKit));
        facesContext.getViewRoot().setRenderKitId(ExtValTrinidadRenderKit.ID);
    }

    public PhaseId getPhaseId()
    {
        return PhaseId.RENDER_RESPONSE;
    }

    /*
     * generated
     */
    @Override
    public boolean equals(Object o)
    {
        if (this == o)
        {
            return true;
        }
        if (!(o instanceof ExtValTrinidadValidationPhaseListener))
        {
            return false;
        }

        return true;
    }

    @Override
    public int hashCode()
    {
        return super.hashCode();
    }
}
