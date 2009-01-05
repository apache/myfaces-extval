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

import org.apache.myfaces.trinidad.validator.ClientValidator;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.validator.Validator;
import javax.faces.validator.ValidatorException;
import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import java.io.Serializable;
import java.util.Collection;

/**
 * in case of client-side validation a trinidad client validator is added to the component based on the meta-data.
 * at the postback: the extval validation strategy gets called and after that the added validator.
 * this wrapper prevents such a server-side double validation.
 * it just delegates the client-side functionality.
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.REUSE)
public class ExtValTrinidadClientValidatorWrapper implements Validator, ClientValidator, Serializable
{
    private ClientValidator wrapped;
    private static final long serialVersionUID = 1414547841700621410L;

    public ExtValTrinidadClientValidatorWrapper(ClientValidator clientValidator)
    {
        this.wrapped = clientValidator;
    }

    public void validate(FacesContext facesContext, UIComponent uiComponent, Object o) throws ValidatorException
    {
        //don't validate - the extval validation strategy will do that
    }

    public String getClientLibrarySource(FacesContext facesContext)
    {
        return wrapped.getClientLibrarySource(facesContext);
    }

    public Collection<String> getClientImportNames()
    {
        return wrapped.getClientImportNames();
    }

    public String getClientScript(FacesContext facesContext, UIComponent uiComponent)
    {
        return wrapped.getClientScript(facesContext, uiComponent);
    }

    public String getClientValidation(FacesContext facesContext, UIComponent uiComponent)
    {
        return wrapped.getClientValidation(facesContext, uiComponent);
    }
}
