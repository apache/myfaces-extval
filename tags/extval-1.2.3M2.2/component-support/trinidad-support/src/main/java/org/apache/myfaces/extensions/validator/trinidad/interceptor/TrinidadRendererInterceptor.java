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
package org.apache.myfaces.extensions.validator.trinidad.interceptor;

import org.apache.myfaces.extensions.validator.core.interceptor.AbstractRendererInterceptor;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;
import org.apache.myfaces.extensions.validator.trinidad.util.TrinidadUtils;
import org.apache.myfaces.trinidad.component.core.output.CoreOutputLabel;

import javax.faces.render.Renderer;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.io.IOException;
import java.util.Map;

/**
 * @author Gerhard Petracek
 * @since 1.x.2
 */
public class TrinidadRendererInterceptor extends AbstractRendererInterceptor
{
    public void beforeEncodeBegin(FacesContext facesContext, UIComponent uiComponent, Renderer wrapped)
            throws IOException
    {
        if(filterCoreOutputLabel(uiComponent))
        {
            initCoreOutputLabel(facesContext, (CoreOutputLabel)uiComponent);
        }
    }

    private boolean filterCoreOutputLabel(UIComponent uiComponent)
    {
        return (uiComponent instanceof CoreOutputLabel);
    }

    protected void initCoreOutputLabel(FacesContext facesContext, CoreOutputLabel coreOutputLabel)
    {
        UIComponent targetComponent = TrinidadUtils.findLabeledEditableComponent(coreOutputLabel);

        if(targetComponent == null || !isComponentEditable(targetComponent))
        {
            return;
        }

        Map<String, Object> metaDataResult = ExtValUtils.getTransformedMetaData(facesContext, targetComponent);

        //get component initializer for the current component and configure it
        //also in case of skipped validation to reset e.g. the required attribute
        if(!metaDataResult.isEmpty())
        {
            ExtValUtils.configureComponentWithMetaData(facesContext, coreOutputLabel, metaDataResult);
        }
    }

    private boolean isComponentEditable(UIComponent uiComponent)
    {
        //compare with false so true = true or null
        boolean isReadOnly = !Boolean.FALSE.equals(ReflectionUtils.tryToInvokeMethod(
                uiComponent, ReflectionUtils.tryToGetMethod(uiComponent.getClass(), "isReadOnly")));
        boolean isDisabled = !Boolean.FALSE.equals(ReflectionUtils.tryToInvokeMethod(
                uiComponent, ReflectionUtils.tryToGetMethod(uiComponent.getClass(), "isDisabled")));

        return !(isReadOnly || isDisabled);
    }
}
