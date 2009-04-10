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
package org.apache.myfaces.extensions.validator.trinidad.util;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.trinidad.component.core.output.CoreOutputLabel;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.component.UIComponent;
import javax.faces.component.EditableValueHolder;
import javax.faces.context.FacesContext;

/**
 * @author Gerhard Petracek
 * @since 1.x.2
 */
@UsageInformation(UsageCategory.INTERNAL)
@ToDo(value = Priority.MEDIUM, description = "check subform")
public class TrinidadUtils
{
    protected static final Log LOG = LogFactory.getLog(TrinidadUtils.class);

    public static UIComponent findLabeledEditableComponent(CoreOutputLabel coreOutputLabel)
    {
        //TODO
        FacesContext facesContext = FacesContext.getCurrentInstance();
        UIComponent result = facesContext.getViewRoot().findComponent(coreOutputLabel.getFor());

        if(result instanceof EditableValueHolder)
        {
            return result;
        }

        if(LOG.isTraceEnabled())
        {
            LOG.trace(coreOutputLabel.getClientId(facesContext) + " doesn't reference an editable component");
        }

        return null;
    }
}
