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
package org.apache.myfaces.extensions.validator.core.initializer.component;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.LogUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultComponentInitializer implements ComponentInitializer
{
    private static List<ComponentInitializer> componentInitializers = new ArrayList<ComponentInitializer>();

    public void configureComponent(FacesContext facesContext, UIComponent uiComponent, Map<String, Object> metaData)
    {
        for(ComponentInitializer componentInitializer : componentInitializers)
        {
            componentInitializer.configureComponent(facesContext, uiComponent, metaData);
            LogUtils.trace("configureComponent of " + componentInitializer.getClass().getName() + " called",getClass());
        }
    }

    @UsageInformation(UsageCategory.INTERNAL)
    public static void addComponentInitializer(ComponentInitializer componentInitializer)
    {
        synchronized (DefaultComponentInitializer.class)
        {
            componentInitializers.add(componentInitializer);
        }
    }
}
