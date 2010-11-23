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
package org.apache.myfaces.extensions.validator.trinidad.initializer.component;

import org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer;
import org.apache.myfaces.extensions.validator.core.InvocationOrder;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.trinidad.storage.TrinidadClientValidatorStorage;
import org.apache.myfaces.trinidad.context.RequestContext;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@InvocationOrder(300)
@UsageInformation(UsageCategory.INTERNAL)
public class TrinidadComponentInitializer implements ComponentInitializer
{
    private static final String TRINIDAD_CORE_INPUT_TEXT
                                         = "org.apache.myfaces.trinidad.component.core.input.CoreInputText";
    private static final String TRINIDAD_CORE_INPUT_DATE
                                         = "org.apache.myfaces.trinidad.component.core.input.CoreInputDate";

    private static List<TrinidadComponentInitializer> componentInitializers =
        new ArrayList<TrinidadComponentInitializer>();

    static
    {
        componentInitializers.add(new RequiredInitializer());
        componentInitializers.add(new LengthInitializer());
        componentInitializers.add(new LongRangeInitializer());
        componentInitializers.add(new DoubleRangeInitializer());
        componentInitializers.add(new PatternInitializer());
        //componentInitializers.add(new ValidatorInitializer());
    }

    @ToDo(value = Priority.LOW, description = "check ppr issue")
    public final void configureComponent(FacesContext facesContext, UIComponent uiComponent,
                                         Map<String, Object> metaData)
    {
        TrinidadClientValidatorStorage storage = ExtValUtils
                .getStorage(TrinidadClientValidatorStorage.class, TrinidadClientValidatorStorage.class.getName());

        for(TrinidadComponentInitializer componentInitializer : componentInitializers)
        {
            if(componentInitializer.configureTrinidadComponent(facesContext, uiComponent, metaData))
            {
                storage.addComponent(uiComponent);
                updateComponent(facesContext, uiComponent);
            }
        }
    }

    protected boolean configureTrinidadComponent(FacesContext facesContext, UIComponent uiComponent,
                                                 Map<String, Object> metaData)
    {
        return false;
    }

    protected boolean processComponent(UIComponent uiComponent)
    {
        return TRINIDAD_CORE_INPUT_TEXT.equals(uiComponent.getClass().getName()) ||
               TRINIDAD_CORE_INPUT_DATE.equals(uiComponent.getClass().getName());
    }

    private void updateComponent(FacesContext facesContext, UIComponent uiComponent)
    {
        if(RequestContext.getCurrentInstance().isPartialRequest(facesContext))
        {
            RequestContext.getCurrentInstance().addPartialTarget(uiComponent.getParent());
        }
    }
}