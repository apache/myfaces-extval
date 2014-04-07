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
package org.apache.myfaces.extensions.validator.core.factory;

import javax.el.MethodExpression;
import javax.faces.component.behavior.AjaxBehavior;
import javax.faces.context.FacesContext;
import javax.faces.event.AjaxBehaviorListener;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ExtValAjaxBehavior extends AjaxBehavior
{
    private Set<MethodExpression> listenerExpressions;

    @Override
    public void addAjaxBehaviorListener(AjaxBehaviorListener listener)
    {
        super.addAjaxBehaviorListener(listener);
        if (this.listenerExpressions == null)
        {
            this.listenerExpressions = new HashSet<MethodExpression>();
        }
        this.listenerExpressions.addAll(extractListenerExpressions(listener));
    }

    @Override
    public void removeAjaxBehaviorListener(AjaxBehaviorListener listener)
    {
        super.removeAjaxBehaviorListener(listener);
        if (this.listenerExpressions != null)
        {
            for (MethodExpression expressionToRemove : extractListenerExpressions(listener))
            {
                this.listenerExpressions.remove(expressionToRemove);
            }
        }
    }

    public List<MethodExpression> getListenerExpressions()
    {
        if (this.listenerExpressions == null)
        {
            return Collections.emptyList();
        }

        return new ArrayList<MethodExpression>(this.listenerExpressions);
    }

    @Override
    public void restoreState(FacesContext facesContext, Object o)
    {
        Object[] state = (Object[]) o;

        if (state[0] != null)
        {
            this.listenerExpressions = (Set<MethodExpression>) state[0];
        }

        super.restoreState(facesContext, state[1]);
    }

    @Override
    public Object saveState(FacesContext facesContext)
    {
        Object[] result = new Object[2];

        result[0] = this.listenerExpressions;
        result[1] = super.saveState(facesContext);
        return result;
    }

    //workaround needed due to a missing api
    private static List<MethodExpression> extractListenerExpressions(AjaxBehaviorListener listener)
    {
        List<MethodExpression> result = new ArrayList<MethodExpression>();
        for (Field field : listener.getClass().getDeclaredFields())
        {
            field.setAccessible(true);
            try
            {
                Object fieldValue = field.get(listener);

                if (fieldValue instanceof MethodExpression)
                {
                    result.add((MethodExpression) fieldValue);
                }
            }
            catch (IllegalAccessException e)
            {
                throw new RuntimeException(e);
            }
        }
        return result;
    }
}
